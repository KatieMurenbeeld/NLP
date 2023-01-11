library(RCurl)
library(XML)
library(stringr)
library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(pdftools)
library(tm)
library(reshape2)
library(wordcloud)
library(forcats)
library(topicmodels)

## Date: 11 January 2023
## Testing out content analysis and topic modeling with the VIP Fall 2022 news articles

## First, load all of the pdf news articles into a list of pdf files

files <- list.files(pattern = "pdf$")
files
length(files)
## Use lapply to to extract the text from all of the pdf files in the list 
## In this case the files are all in the working directory
vip <- lapply(files, pdf_text) 

lapply(vip, length)
length(vip)

## Next, build the corpus using the Corpus() function from the tm package

corp <- Corpus(URISource(files), # First argument is what we want to use to create the corpus, the vector of pdf files. URI (uniform resource identifier) source means that the vector of file names identifies our resources
               readerControl = list(reader = readPDF)) # Second argument requires a list of control parameters, one is reader, to read in PDF files, same in tm as pdftools package

## From this corpus, create a document term matrix.
## The DTM will be useful for topic modeling but can be converted into a tidy data frame 
# for descriptive and exploratory analysis

vip.dtm <- DocumentTermMatrix(corp,
                              control = 
                                list(removePunctuation = TRUE,
                                     stopwords = TRUE,
                                     tolower = TRUE,
                                     stemming = FALSE,
                                     removeNumbers = FALSE,
                                     bounds = list(global = c(1, Inf))))
vip.dtm
inspect(vip.dtm)

## Create a tidy data frame of the corpus

vip.tidy <- tidy(vip.dtm)
vip.tidy

## What are the most common words in the corpus?

vip.tidy %>%
  count(term, sort = TRUE)
# notice that "2022", "copyright", and "page" are in every document (n = 47). These are other
# possible words that could be removed
# Also these are relative short articles so the term/word counts are fairly low

## Let's remove "2022", "copyright", and "page"

#vip.tidy <- vip.tidy %>%
#  anti_join()

vip.tidy %>%
  count(term, sort = TRUE) %>%
  filter(n > 10) %>% 
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term)) +
  geom_col() +
  labs(y = NULL)
  
## Sentiment Analysis

vip.sent <- vip.tidy %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
  count(document, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)
  
ggplot(vip.sent, aes(document, sentiment, fill = sentiment)) + 
  geom_col(show.legend = FALSE)

# might be useful to figure out how to rename the documents or create a new column to help identify them?

# What are the most common positive and negative words?

vip.bing.counts <- vip.tidy %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
  count(term, sentiment, sort = TRUE) %>%
  ungroup()
vip.bing.counts

vip.bing.counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", 
       y = NULL)

# We may want to do some stemming? For example "killed", "kill", "killing", "bears", "bear"
# Also, may need to look into other sentiment lexicons or make our own?
# Can also make a comparison word cloud

vip.tidy %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
  count(term, sentiment, sort = TRUE) %>%
  acast(term ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

## Analyzing word and document frequency: tf-idf
vip.tidy

# We need to order the table by count first
vip.words <- vip.tidy %>%
  arrange(desc(count))
vip.words

vip.words['animal'] <- NA

# If document has "bear" I want bear in the animal column and if document has "wolf" or "wolve" I want wolf in the animal column

vip.words$document <- tolower(vip.words$document)
vip.words$animal <- ifelse(str_detect(vip.words$document, "bear") == TRUE, "bear", "wolf")
# I need to work on that more, good enough for now to test things out

vip.words %>% count(animal)

total.words <- vip.words %>%
  group_by(animal) %>%
  summarise(total = sum(count))

vip.words <- left_join(vip.words, total.words)
vip.words

vip.freq.rank <- vip.words %>%
  group_by(animal) %>%
  mutate(rank = row_number(),
         'term_frequency' = count/total) %>%
  ungroup()

vip.freq.rank %>%
  ggplot(aes(rank, term_frequency, color = animal)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()

vip.tf.idf <- vip.words %>%
  bind_tf_idf(term, document, count)
vip.tf.idf

vip.tf.idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

vip.tf.idf %>%
  group_by(document) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  mutate(term = reorder(term, tf_idf)) %>%
  ggplot(aes(tf_idf, term, fill = document)) + 
  geom_col(show.legend = FALSE) + 
  labs(x = "tf-idf", y = NULL) + 
  facet_wrap(~document, ncol = 5, scales = "free")


## Do I want to mess with bigrams?
## If so I need to figure out how to get these pdfs in here so that I can use unnest_tokens()
## I don't think a huge issue for now. The title and newspaper are being included.

## I want to move on to testing the LDA. Can think more about text mining later.

## Here we want to use the vip.dtm to run the LDA
## Just two topics for now to see if it can find the bear and wolf articles

vip.lda <- LDA(vip.dtm, k = 2, control = list(seed = 1234))
vip.lda

vip.topics <- tidy(vip.lda, matrix = "beta")
vip.topics

vip.top.terms <- vip.topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta) # arrange sorts the data

vip.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered()


# Below doesn't tell us as much as the straight beta values
# Below is the log ratio of words in topics 1 and 2. 
# The words with the greatest differences between the two topics 
# (so if occurring in topic2 very unlikely to occur in topic1 and vice versa)

vip.beta.wide <- vip.topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

vip.beta.wide %>%
  arrange(desc(abs(log_ratio))) %>%
  head(20) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) + 
  geom_col(show.legend = FALSE) + 
  scale_y_reordered()

vip.docs <- tidy(vip.lda, matrix = "gamma")
vip.docs

vip.docs %>%
  mutate(titel = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))

## Very little overlap or misscalssified news articles with only two topics. 

## Let's test with more topics!




