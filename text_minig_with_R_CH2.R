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
library(wordcloud)
library(reshape2)

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

# 2.2 Comparing the three sentiment dictionaries

# Using all three sentiment lexicons, how does sentiment change across
# the narrative arc of Pride & Prejudice?

pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")


afinn <- pride_prejudice %>% # define a new variable, assign to pride_prejudice
  inner_join(get_sentiments("afinn")) %>% # join the afinn sentiment lexi.
  group_by(index = linenumber %/% 80) %>% # group text by 80 line numbers, creates a column "index" 
  summarise(sentiment = sum(value)) %>% # sum the sentiment value for each text chunk and add to column sentiment
  mutate(method = "AFINN") # create a new column "method" and fill each row with "AFINN"

bing_and_nrc <- bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative")) # this gets the sentiments from NRC that are labeled positive or negative
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(sentiment = positive - negative)


# Combine (bind) the 2 dataframes created above and plot

bind_rows(afinn,            # combine the afinn and bing_and_nrc dataframe rows
          bing_and_nrc) %>% 
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Why the differences in absolute values?

get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)

# both have more negative than positive words, but the ratio of positive
# to negative words is higher in the Bing than the NRC lexicon

# 2.4 Most common positive and negative words

# Analyze the word counts that contribute to each sentiment

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()  # why is ungroup() used here?

# Can easily visualize the positive and negative word counts by piping to ggplot

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% # select rows by the highest values of a variable, here top ten
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(x = "Contribution of sentiment",
      y = NULL)

# 2.5 Word Clouds!!

# Visualize the most common words in Jane Austen's works using a wordcloud

tidy_books %>%
  anti_join(stop_words) %>% # take out stop words
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Note, you will get a few warnings if there isn't
# enough room in the plot window to fit all the words

# For other functions, you may need to turn the dataframe into a matrix
# Use the acast() function from the reshape2 library

tidy_books %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# 2.6 Looking at units beyond just words

# If we want to analyze the sentiment of sentences we will have to 
# tokenize the text for sentences

p_and_p_sentences <- tibble(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")

# the sentence tokenizing has trouble with UTC-8 encoded text
# ASCII handles punctuation better
# could convert the text using iconv(), for example iconv(text, to = "latin1")
# in a mutate statement before unnesting

# OR we can split into tokens using a regex pattern
# here, split the Jane Austen text into a dataframe by chapter

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVXVC]") %>%
  ungroup()

austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n())

# What are the most negative chapters in each of Jane Austen's novels?

# First, using the Bing lexicon, get all of the negative words using filter()

bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

# Second, make a dataframe of word count for each chapter so we can 
# normalize for the length of chapters

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

# Third, find the number of negative words in each chapter and divide 
# by the total words in each chapter 


tidy_books %>%
  semi_join(bingnegative) %>% # semi_join is a filtering join, filter rows from x based on the presence of a match in y
  group_by(book, chapter) %>%
  summarise(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  slice_max(ratio, n = 1) %>% # show the most negative (highest ratio value) for each chapter
  ungroup()
  
  
  
