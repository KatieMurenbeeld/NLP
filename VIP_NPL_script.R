# Highlight arguments and options for functions that students can change

# For the first part of the script (the web scraping) only load these libraries
library(httr) # for accessing the html from the link
library(stringr) # for extracting the relevant text
library(dplyr) # package to help manipulate data frames
library(googlesheets4) # package to download google sheets

# Set your working directory
#setwd()

# Can read in a csv or use googlesheets4 (googlesheets4 would then always let you have the most recent version of the article coding) 
# read in the csv with the urls for the articles
article_codes <- read.csv(file = 'article_coding_20230221.csv')

# read in the csv with googlesheets4. Need to have something with permissions....
#article_codes <- read_sheet('https://docs.google.com/spreadsheets/d/1of5iDf_SF7pHCtXuwpQAGnAhEcOENGwIWcaRg38uAUg/edit#gid=0')

# remove any rows without a link in the Link column and create an article ID column
article_codes <- article_codes %>% 
  filter(!Link=='') %>% 
  mutate(id = row_number())

# create a vector or list of the urls from the csv
urls <- article_codes$Link

# for each url:
# 1) use httr's GET() function to request the url
# 2) use httr's content() function to access the body of the requested webpage as text
# 3) use stringr's str_extract_all() function to extract the text between the <p></p> 
# <p></p> is the html paragraph element and contains the text of interest for the article
# make sure to set simplify = TRUE
# 4) #3 will result in a 3 column table. We want the second column

# For testing
# urls_test <- urls[1:10]

# Before running the for loop, create an empty data frame 
df_article <- data.frame()

### IMPORTANT!!! Must be connected to VPN or on campus for this to work! ###

for (url in urls){ # this was working, but now I am getting an error: Error in content(page, as = "text"): unused argument ("text")
  if(url == "") next
  link <- url
  #if(link == "") next
  page <- GET(link)
  page_text <- content(page, as = 'text')
  text_body <- str_extract_all(page_text, regex("(?<=<p>).+(?=</p>)"), simplify = TRUE) 
  text <- text_body[[2]]
  df_article <- rbind(df_article, text)
}

# Rename and reorder the data frame columns
colnames(df_article) <- c("article.text")
df_article <- df_article %>% mutate(id = row_number()) %>% select(id, article.text)

# from the article_codes data frame select one or more variables/columns you would like to join to df_article
# you can update the columns in select() to include whatever information you'd like
# for example, article type, newspaper, etc. But you need the "id" column so that you can "join" the 2 data frames
df_to_join <- article_codes %>% select(id, Species) 
df_article <- df_article %>% full_join(df_to_join, by = "id")

# Create a tidy data frame and create some plots!

# Load the rest of the libraries you will need for text analysis and topic modeling
# If you load these first when trying to use httr::content() in lines 39-48 you will get this error: Error in content(page, as = "text"): unused argument ("text") 
library(tidyverse) # an "opinionated" collection of packages. Includes, dplyr, ggplot2, forcats, tibble, readr, stringr, tidyr, and purr
library(tidytext) # for creating a tidy dataframe from the website text data
library(ggplot2) # package for plotting
library(wordcloud) # package for creating word clouds
library(reshape2) # package to restructure and aggregate data
library(tm) # package for topic modeling
library(topicmodels) # package for topic modeling
library(SnowballC) # to stem words
# Make a tidy data frame by "unnesting" the tokens
# This automatically removes punctuation and will lowercase all words
tidy_df <- df_article %>% unnest_tokens(word, article.text)

# Remove "stop words" using the SMART lexicon 
data("stop_words")
tidy_df <- tidy_df %>%
  anti_join(stop_words)

# Create a small data frame of your own stop words for this project 
# ("br" and "strong" are part of the html formatting which should come out before making tidy df!)
# This needs to be a data frame so that you can use "anti-join()"
# You can add your own words to this list
my_stop_words <- data.frame(c("br", "strong")) 
colnames(my_stop_words) <-("word")

tidy_df <- tidy_df %>%
  anti_join(my_stop_words)

# Can create separate data frames for each species using filter()
tidy_df_bears <- tidy_df %>%
  filter(Species == "Grizzly Bear" | Species == "Grizzly bear")
tidy_df_beavers <- tidy_df %>%
  filter(Species == "Beavers" | Species == "beavers")
tidy_df_boars <- tidy_df %>%
  filter(Species == "Boars")
tidy_df_wolves <- tidy_df %>%
  filter(Species == "Wolves")

# Create a document term matrix. You will need this for topic modeling
# You will need to create a word count for all of the words in a document
dtm <- tidy_df %>% 
  count(id, word) %>%
  cast_dtm(id, word, n)

# For n-grams
# First, from the articles dataframe unnest the tokens, but now we will look at bigrams
# instead of creating a column called "word" we will create a column called "bigram"
# The type of token will be set to token = "ngrams" and the number of words will be set to 2 (n=2)
df_bigrams <- df_article %>%
  unnest_tokens(bigram, article.text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))


# In order to remove the stop words we have to first separate out the two words into word1 and word2
bigrams_separated <- df_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Then filter out stopwords and our own stop words for word1 and word2
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>% 
  mutate(stem1 = wordStem(word1)) %>% # not sure if we want to stem the words for bigrams or not, but would do so here.
  mutate(stem2 = wordStem(word2))

# Then, calculate new bigram counts. This is optional, but good to check. 
bigram_counts <- bigrams_filtered %>% 
#  count(word1, word2, sort = TRUE)
  count(stem1, stem2, sort = TRUE)

# Finally, reunite word1 and word2 back into a single bigram separated by a space (sep = " ")
bigrams_united <- bigrams_filtered %>%
#  unite(bigram, word1, word2, sep = " ") %>% 
  unite(bigram, stem1, stem2, sep = " ")

# From the united bigrams, we can then create a document term matrix
dtm_bigrams <- bigrams_united %>% 
  count(id, bigram) %>%
  cast_dtm(id, bigram, n)

# Can create separate dtms for each species by filtering for species before generating the dtm
dtm_bears <- tidy_df %>% 
  filter(Species == "Grizzly Bear" | Species == "Grizzly bear") %>%
  count(id, word) %>%
  cast_dtm(id, word, n)
dtm_beavers <- tidy_df %>% 
  filter(Species == "Beavers" | Species == "beavers") %>%
  count(id, word) %>%
  cast_dtm(id, word, n)
dtm_boars <- tidy_df %>% 
  filter(Species == "Boars") %>%
  count(id, word) %>%
  cast_dtm(id, word, n)
dtm_wolves <- tidy_df %>% 
  filter(Species == "Wolves") %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

## Plotting

# Plot the most common words in the tidy data frame
tidy_df %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n=20) %>%  # set the number of words to show
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(title = "20 Most Common Words in the VIP Corpus", # can change the title of the plot
       y = NULL)

bigrams_united %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n=20) %>%  # set the number of words to show
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(n, bigram)) +
  geom_col() +
  labs(title = "20 Most Common Bigrams in the VIP Corpus", # can change the title of the plot
       y = NULL)

# I'd really like to use facet_wrap here for species, but not sure how to make sure to keep Species variable when counting the words
# tidy_df %>%
#   count(word, sort = TRUE) %>%
#   slice_max(n, n=20) %>%  # set the number of words to show
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(n, word, fill = Species)) +
#   geom_col() +
#   #facet_wrap(~Species, ncol = 2, scales = "free_x") + 
#   labs(title = "20 Most Common Words in the VIP Corpus", # can change the title of the plot
#        y = NULL)

# Can plot for the different species in 2 ways. Can filter() for the species or 
# use a pre-filtered data frames (see lines 89-96)
# Option 1:
#tidy_df %>% filter(Species == "Beavers") %>%

# Option 2: 
tidy_df_bears %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n=20) %>%  # set the number of words to show
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "brown") +
  labs(title = "20 Most Common Words in Articles About Bears", # update the Species
       y = NULL)

tidy_df_beavers %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n=20) %>%  # set the number of words to show
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "blue") +
  labs(title = "20 Most Common Words in Articles About Beavers", # update the Species
       y = NULL)

tidy_df_boars %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n=20) %>%  # set the number of words to show
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "pink") +
  labs(title = "20 Most Common Words in Articles About Boars", # update the Species
       y = NULL)

tidy_df_wolves %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n=20) %>%  # set the number of words to show
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "grey") +
  labs(title = "20 Most Common Words in Articles About Wolves", # update the Species
       y = NULL)

# To plot the most common positive and negative words 

# If you want to do this for specific species...
# You will always need to create a a data frame of sentiment word counts.

# Create data frame of sentiment word counts
sent_all <- tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Use the sentiment word counts data frame to make the plot
sent_all %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  scale_fill_manual(values=c("blue", "pink")) + # change the colors of the bar plot
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", 
       y = NULL,
       title = "Most Common Positive and Negative Words in VIP Corpus")

# Now for bears
sent_bears <- tidy_df_bears %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Use the sentiment word counts data frame to make the plot
sent_bears %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  scale_fill_manual(values=c("tan", "brown")) + # change the colors of the bar plot
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", 
       y = NULL,
       title = "Most Common Positive and Negative Words in Articles About Bears")

# Now for beavers
sent_beavers <- tidy_df_beavers %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Use the sentiment word counts data frame to make the plot
sent_beavers %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  scale_fill_manual(values=c("lightblue", "blue")) + # change the colors of the bar plot
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", 
       y = NULL,
       title = "Most Common Positive and Negative Words in Articles About Beavers")

# Now for boars
sent_boars <- tidy_df_boars %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Use the sentiment word counts data frame to make the plot
sent_boars %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  scale_fill_manual(values=c("pink", "red")) + # change the colors of the bar plot
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", 
       y = NULL,
       title = "Most Common Positive and Negative Words in Articles About Boars")

# Now for wolves
sent_wolves <- tidy_df_wolves %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Use the sentiment word counts data frame to make the plot
sent_wolves %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  scale_fill_manual(values=c("gray20", "gray80")) + # change the colors of the bar plot
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", 
       y = NULL,
       title = "Most Common Positive and Negative Words in Articles About Wolves")

# Word clouds
# Can do for each animal or for the entire corpus
tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# Overall Sentiment of Articles
overall_sent <- tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(overall_sent, aes(id, sentiment, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  labs(title = "Document Sentiment: Bing Lexicon")

# Now for bears
overall_sent_bears <- tidy_df_bears %>%
  inner_join(get_sentiments("bing")) %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(overall_sent_bears, aes(id, sentiment, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  labs(title = "Document Sentiment: Articles About Bears")

# Now for beavers
overall_sent_beavers <- tidy_df_beavers %>%
  inner_join(get_sentiments("bing")) %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(overall_sent_beavers, aes(id, sentiment, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  labs(title = "Document Sentiment: Articles About Beavers")

# Now for boars
overall_sent_boars <- tidy_df_boars %>%
  inner_join(get_sentiments("bing")) %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(overall_sent_boars, aes(id, sentiment, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  labs(title = "Document Sentiment: Articles About Boars")

# Now for wolves
overall_sent_wolves <- tidy_df_wolves %>%
  inner_join(get_sentiments("bing")) %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(overall_sent_wolves, aes(id, sentiment, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  labs(title = "Document Sentiment: Articles About Wolves")


## Modeling 

# Here you can update the k value which sets the number of topics to model
lda <- LDA(dtm, k = 4, control = list(seed = 1234))
lda_bears <- LDA(dtm_bears, k = 2, control = list(seed = 1234))
lda_beavers <- LDA(dtm_beavers, k = 2, control = list(seed = 1234))
lda_boars <- LDA(dtm_boars, k = 2, control = list(seed = 1234))
lda_wolves <- LDA(dtm_wolves, k = 2, control = list(seed = 1234))


# Get the beta values for each term
topics <- tidy(lda, matrix = "beta")

terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  # arrange sorts the data
  arrange(topic, -beta) 

terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered() + 
  labs(title = "Term Beta Values for 4 Topics")

# Repeat for each species
topics_bears <- tidy(lda_bears, matrix = "beta")

terms_bears <- topics_bears %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  # arrange sorts the data
  arrange(topic, -beta) 

terms_bears %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered() + 
  labs(title = "Term Beta Values for 2 Topics")

# Repeat for each species
topics_bears <- tidy(lda_bears, matrix = "beta")

terms_bears <- topics_bears %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  # arrange sorts the data
  arrange(topic, -beta) 

terms_bears %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered() + 
  labs(title = "Term Beta Values for 2 Topics: Bears")

# Beavers
topics_beavers <- tidy(lda_beavers, matrix = "beta")

terms_beavers <- topics_beavers %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  # arrange sorts the data
  arrange(topic, -beta) 

terms_beavers %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered() + 
  labs(title = "Term Beta Values for 2 Topics: Beavers")

# Boars
topics_boars <- tidy(lda_boars, matrix = "beta")

terms_boars <- topics_boars %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  # arrange sorts the data
  arrange(topic, -beta) 

terms_boars %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered() + 
  labs(title = "Term Beta Values for 2 Topics: Boars")

# Wolves
topics_wolves <- tidy(lda_wolves, matrix = "beta")

terms_wolves <- topics_wolves %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  # arrange sorts the data
  arrange(topic, -beta) 

terms_wolves %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered() + 
  labs(title = "Term Beta Values for 2 Topics: Wolves")
