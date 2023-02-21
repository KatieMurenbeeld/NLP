# From "Text Mining with R: A Tidy Approach" by Julia Silge & David Robinson



## Load the appropriate libraries
library(dplyr) # to transform data into "tidy" data
library(tidytext) # for text mining
library(janeaustenr) # text from Jane Austen's 6 completed, published novels
library(stringr) # makes working with strings easier
library(ggplot2) # for data visualization
library(ggraph) # extends ggplot2 to construct network plots
library(widyr) # calculates pairwise correlations and distances within a tidy data frame
library(tidyr)
library(forcats) # I think needed for fct_reorder
library(tm)
library(topicmodels)

# 6. Topic Modeling (yay!)


# 6.1 Latent Dirichlet allocation

data("AssociatedPress")
AssociatedPress

## set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

## 6.1.1 Word-topic probabilities

## We want a tidy data frame of the LDA results
## We will need the per-topic-per-word probabilities ("beta")

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

## Now the model is a one-topic-per-term-per-row format, i.e. a tidy format

## Use dplyr's slice_max() to find the 10 terms that are most common within each topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms

## The tidy format also makes it relatively easy to visualize
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered()

## This helps us to understand the words that contribute to each topic and to see any words that
## occur in both topics

## We could also consider the terms that had the greatest difference in beta between topic 1 and 2.
## We use the log ratio of beta from topic 2/ beta from topic 1.
## Beta2 being twice as large as Beta1 has a log ratio of 1
## Beta1 being twice as large as Beta2 has a log ratio of 2
## We can filter for relatively common words, such as those with a beta > 1/1000 in at least one topic

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>% # paste0, concatenate strings without spaces, leads to "topic1" and "topic2"
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide

## Try to recreate figure 6.3
beta_wide %>%
  arrange(desc(abs(log_ratio))) %>%
  head(20) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) + 
  geom_col(show.legend = FALSE) + 
  scale_y_reordered()
  
## 6.1.2 Document-topic probabilities

## LDA also models each document as a mixutre of topics. 
## We can examine the per-document-per-topic probabilities, gamma

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents  

## Each of the gamma values is an estimated proportion of the words from that 
## document that are generated from that topic. 
## About 25% of the words in document 1 were generated from topic 1 (gamma = 0.248).
## And 75% of the words in document 1 were generated from topic 2.

ap_documents %>% 
  filter(document == 6)

## We can see that document was drawn almost entirely from topic 2 (gamma = 0.999)
## To chek this, we can tidy the DTM and see what the most common words in doc 6 were.

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

## This article was about US-Panama relations, so the algorithm correctly placed it in topic 2.

# 6.2 Example: the great library heist

## This uses guntenbergr, so I will try this with 4 books from Jane Austen.

titles <- c("Sense & Sensibility", "Emma", "Northanger Abbey", "Mansfield Park")

books <- austen_books() %>%
  filter(book %in% titles)

# divide into documents, each representing one chapter

by_chapter <- books %>%
  group_by(book) %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^chapter ", ignore_case = TRUE)
  ))) %>%
  ungroup() %>%
  filter(chapter > 0 ) %>%
  unite(document, book, chapter)

# Split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# Find the document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE)

word_counts

## 6.2.1 LDA on chapters

## word_counts is in tidy format, but the topicmodels requires a DTM
## So we can use cast_dtm() to put word_counts into a DTM format

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)
chapters_dtm

## We can then use LDA() to create a 4 topic model. In this case we know we want 
## 4 topics because we have 4 books. 

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

## Let's examine per-topic-per-word probabilities (beta)

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

## chapter_topics is now tidy, with a one-topic-per-term-per-row format. 

## Find the top 5 terms within each topic

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

## Let's visualize! Remember tidy data is relatively easy to use with ggplot2

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered()

## Since Jane Austen novels are so character driven it makes sense that
## each topic is so clearly associated with each book.

## 6.2.2 Per-document classification

## Which topics are associated with each document? Can we put the chapters back together 
## in the correct books?
## We can find this by examining the per-document-per-topic probabilities (gamma)

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

## With these topic probabilities we can see how well our unsupervised learning did 
## at distinguishing the four books. The chapters within each book should be mostly
## or entirely generated from the corresponding topic. 

## First, re-separate the document name into title and chapter, 
## then visualize the per-document-per-topic probability

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_gamma

## reorder titles in order of topic 1, topic 2, etc before plotting

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>% #why the *?
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) + 
  labs(x = "topic", y = expression(gamma))

## In general the books correspond to the correct topics. 
## Emma is mostly topic 1, which is correct, but quite a few chapters were also
## classed as topic 3, which should correspond to Northanger Abbey.

## What are the cases where the topic most associated with a chapter belonged 
## to another book? 
## To answer, first find the topic that was most associated with each chapter using
## slice_max(), which is effectively the "classification" of that chapter.

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  slice_max(gamma) %>%
  ungroup()

## Why is ungroup() always used after group() (or group_by())? 
## If you forget to ungroup() the data, future data management will likely
## produce errors. Always ungroup() when you've finished with your calculations!
## The above is from https://bookdown.org/yih_huynh/Guide-to-R-Book/groupby.html#ungrouping 
## which may be a helpful resource. 

chapter_classifications

## We can then compute each to the "consensus" topic for each book (the most common
## topic among its chapters), and see which were most often misidentified.

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  slice_max(n, n = 1) %>%
  ungroup() %>%
  transmute(consensus = title, topic) 
# transmute is new to me. #transmute() adds a new variables and drops exisiting ones
# mutate() adds new variables and preserves existing ones

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

## Only two chapters from Northanger Abbey were misclassified, the LDA describes
## them both as coming from Mansfield Park.

## 6.2.3 By word assignments: augment

## The LDA algorithm assigns each word in each document to a topic. The more
## words in a documents assigned to that topic, the more weight (gamma) will
## go on that document-topic classification.
## How to find which words in each document were assigned to which topic?
## augment(), part of the broom package, uses a model to add information to 
## each observation in the original data. 

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

## can then combine the assignments table with the consensus book titles to
## find which words were incorrectly classified.

assignments <- assignments %>%
  separate(document, c("title", "chapter"),
           sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments

## With this data (true book title and assigned/predicted book title) we can 
## make a confusion matrix, showing how often words from one book
## were assigned to another

library(scales)

assignments %>%
  count(title, consensus, wt = count) %>%
  mutate(across(c(title, consensus), ~str_wrap(., 20))) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) + 
  geom_tile() + 
  scale_fill_gradient2(high = "darkred", label = percent_format()) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to", 
       y = "Book words came from", 
       fill = "% of assignemnts")

## What were the most commonly mistaken words?

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

word_counts %>%
  filter(word == "wilderness")

## In a few cases there are a few wrongly classified words that never appeared
## in the novel they were assigned to. For example "wilderness" only appears
## in Mansfield Park, not in Emma. 
## The LDA algorithm is stochastic, and it can accidentally land on a topic
## that spans multiple books. 

# 6.3 Alternative LDA implementations

## There is another implementation of LDA using the mallet package.
## Mallet takes in non-tokenized documents, performs the tokenization itself, 
## and requires a separate file of stopwords. 

## We need to collapse the text into one string for each document before 
## performing the LDA.

library(mallet)

## create a vector with one string per chapter
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

## create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

## Once the model is created, we can use the tidy() and augment() functions 
## roughly the same way. Including extracting the probabilities of words
## within each topic or topics within each document.

## word-topic pairs
tidy(mallet_model)

## document-topic pairs
tidy(mallet_model, matrix = "gamma")

## column needs to be named "term" or "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)

## Then we can use ggplot2 to visualize the model in the same way as the LDA() 


