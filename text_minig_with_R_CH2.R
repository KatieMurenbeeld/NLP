# From "Text Mining with R: A Tidy Approach" by Julia Silge & David Robinson



## Load the appropriate libraries
library(dplyr) # to transform data into "tidy" data
library(tidytext) # for text mining
library(janeaustenr) # text from Jane Austen's 6 completed, published novels
library(stringr) # makes working with strings easier
library(ggplot2) # for data visualization
library(scales)
library(tidyr)
library(textdata)

# 2. Sentiment analysis with tidy data

# Sentiment, is something positive or negative, 
# or even angry, sad, joyful, fear, trust

# For these examples we will look at a chunk of text
# and add up the sentiment for all of the single words
# found within a sentiment lexicon

# 2.1 The sentiments datasets

# Tidytext provides access to several sentiment lexicons
# Lexicons require different licenses
# Make sure the lincese for your lexicon of choice is appropriate

## Get the sentiments from lexicons AFINN, bing, and nrc

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# These lexicons are based on unigrams (single words)
# They do not take into account qualifiers like "no" or "not"
# The size of the text chunk we use to add up unigram sentiment
# can impact the results of the analysis
# Sentence- or paragraph-sized chunks are good

# 2.2 Sentiment analysis with inner join

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
# helpful to name the output column "word" when using unnest
# because the stop word and sentiment lexicon datasets have
# columns named "word" making inner joins and anti-joins easier

# Get the words representing a sentiment of joy
# from the NRC sentiments lexicon dataset
# Using the filter() function

nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

# Get the words from Emma and then join with the 
# nrc_joy variable
# then count the number of joyful words in the book Emma

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# How does sentiment change throughout each of Jane Austen's
# novels?
# Here we will use the Bing lexicon 
# and look at 80 lines of text as the text chunk

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

# Let's plot

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")





