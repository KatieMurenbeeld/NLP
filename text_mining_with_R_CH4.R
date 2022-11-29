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

# 4.1 Tokenizing by n-gram

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# 4.1.1 Counting and filtering n-grams

austen_bigrams %>%
  count(bigram, sort = TRUE)

## The most common bigrams are pairs of common words, stop-words.
## We can use tidyr separate() function to split a column into multiple columns
## based on a delimiter

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") # here the delimiter is the space between the two words in the bigram column

bigrams_filtered <- bigrams_separated %>% # now we want to filter out the stop words
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

## Names are the most common word pairs in Jane Austen books
## For other analyses we may want to recombine words using unite() which is the inverse of separate()
## separate/filter/count/unite will allow one to find the most common bigrams not containing stop-words

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

## We can also look at trigrams, sequneces of 3 words

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word, 
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

## 4.1.2 Analyzing bigrams

## The tidy format (one-bigram-per-row) is great for exploratory analyses of the text/corpus
## What are the most common streets mentioned in each book

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

## A bigram (or trigram) can be treated as a term, so we can look at term frequency and 
## inverse document frequency (tf-idf see CH 3)

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book, ncol = 2, scales = "free") + 
  labs(x =  "tf-idf", y = NULL)







