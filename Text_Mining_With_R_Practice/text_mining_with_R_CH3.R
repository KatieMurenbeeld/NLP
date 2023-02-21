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

## The frequency that a word appears is inversely proportional to its rank

## Examine Zipf's Law for Jane Austen's novel using dplyr functions

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         term_frequency = n/total) %>%
  ungroup()

## Because book_words was already ordered by n (number of times a word appears)
## we could use row_number()  to find the rank of the word.

## Zipf's Law often visualized with x = rank and y = term frequency
## on a log scale

freq_by_rank %>%
  ggplot(aes(rank, term_frequency, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

## The resulting curves have a negative slope but they are not constant
## This could be viewed as a broken power law with 3 sections
## Look at the exponenet of the power law is for the middle sections of the rank range

rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)

## We do get a slope close to -1 (-0.62)

## Use the slope (-0.62) and intercept (-1.1) 
## to plot the fitted power law

freq_by_rank %>%
  ggplot(aes(rank, term_frequency, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1,
              color = "grey50", linetype = 2) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() + 
  scale_y_log10()

## The deviations at high rank are common, while the deviations 
## low rank are uncommon. Interpretation: Jane Austen uses a lower 
## percentage of the most common words than many collections of language.

# 3.3 The bind_tf_idf() function

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)


## Look at terms with high tf-idf in Jane Austen's work

book_tf_idf %>%
  select(-total) %>% 
  arrange(desc(tf_idf))

## Notice that these are all proper nouns (names of characters)
## These names are important, charactertistic words for each text
## within the Jane Austen corpus


# Let's visualize the 15 most important words in each Jane Austen novel

library(forcats)

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book, ncol = 2, scales = "free") + 
  labs(x =  "tf-idf", y = NULL)

## Interpretation: Jane Austen uses similar language throughout 
## all of her novels, but what distinguishes each novel from 
## the others are the names of the people and places,
## the characters and the locations. 

# 3.4 A corpus of physics texts

## This requires the gutenbergr library 
## which does not work on my version of R












