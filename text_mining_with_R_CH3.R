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

# 3 Analyzing word and document frequency: tf-idf

# 3.1 Term frequency in Jane Austen's novels

# First, examine term frequency in the published works of Jane Austen
# unnest the tokens by book

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>%
  group_by(book) %>%
  summarise(total = sum(n))


book_words <- left_join(book_words, total_words)

# The words, "the", "to", "and" have the highest n 
# n = number of times that word is used in the book
# total = the total number of words in the book

# Let's look at n/total, the number of times a word appears in a novel
# divided by the total number of words in that novel

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) + 
  xlim(NA, 0.0009) + 
  facet_wrap(~book, ncol = 2, scales = "free_y")

# 3.2 Zipf's law

