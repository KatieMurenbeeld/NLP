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

## 4.1.3 Using bigrams to provide context in sentiment analysis

## How often are words preceded by the word "not"?

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

## By doing seniment analysis on bigrams, we can see how often sentiment-associated
## words are preceded by "not". Then we can ignore or reverse those word's contribution
## to the sentiment score (use the AFINN lexicon, remember it scores words with a 
## positive or negative number)

AFINN <- get_sentiments("afinn")

AFINN

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

## Which words contributed the most in the "wrong" sentiment direction?
## We want to multiply the sentiment value of each word by how often 
## it occurs.

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>% # I need to check or ask what happens here.
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) + 
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

## We can look at other negating words as well

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

# I'm close, but I can't quite get the 2x2 subplots
negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>% # I need to check or ask what happens here.
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~word1, ncol = 4, scales = "free") + 
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by negative term")

## 4.1.4 Visualizing a network of bigrams with ggraph
library(igraph)

# original bigram counts
bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Can add some polishing touches to the network figure

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


## 4.1.5 Visualizing bigrmas in other texts

## Can make a function to easily count and visualize bigrams from other
## texts or datasets. (I've never made a function in R before, exciting!)

count_bigrams <- function(dataset) {
  dataset %.%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <-  grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %.%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") + 
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightgreen", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# 4.2 Counting and correlating pairs of words with the widyr package

## Most operations for finding pairwise counts or correlations need to turn the 
## data into a wide matrix first (not tidy)

## widyr simplfies the pattern of widen data -> perform an operation -> re-tidy the data

## 4.2.1 Counting and correlating among sections

## consider Pride and Prejudice divided into 10-line section






