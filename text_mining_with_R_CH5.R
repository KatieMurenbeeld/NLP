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

# 5. Converting to and from non-tidy formats

## The most common data structure for natural language processing is a document-term matrix (DTM)
## DTMs are a (usually sparse) matrix where:
##    each row represents one document (book or article)
##    each column represent one term
##    each value (typically) contains the number of appearances of that term in the document

## Many R tools for NLP require this data structure and won't work with a tidy data format

# 5.1 Tidying a document-term matrix

## can use tidy() to turn a document-term matrix into a tidy data frame
## can use cast() to turn a tidy data frame into a matrix
## note: most DTM are comparable to a tidy data frame after a count() or group_by() or summarize()

# 5.1.1 Tidying DocumentTermMatrix objects

## Most widely used implementation of DTMs in R with the DocumentTermMatrix class in the tm package

## Here we will look at the collection AP newspaper articles included in the topic models package

data("AssociatedPress", package = "topicmodels")

## Note: getting topicmodels on R is a bit of a pain for some reason.

AssociatedPress

## access the terms in the document
terms <- Terms(AssociatedPress)
head(terms)

## turn this into a tidy (one-token-per-document-per-row)
ap_td <- tidy(AssociatedPress)
ap_td

## the tidy data has no rows where the count is 0
## this form is good for analysis with dplyr, tidytext, and ggplot2 packages
## let's perofrm a sentiment analysis 

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
ap_sentiments

## Now we can visualize/plot which words from the AP articles most often contributed to postive 
## or negative sentiment

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() + 
  labs(x = "Contribution to sentiment", y = NULL)

# 5.1.2 Tidying dfm objects

## dfm = document feature matrix class from the quanteda package

data("data_corpus_inaugural", package = "quanteda")

inaug_dfm <- data_corpus_inaugural %>%
  quanteda::tokens() %>%
  quanteda::dfm(verbose = FALSE)
inaug_dfm

inaug_td <- tidy(inaug_dfm)
inaug_td

## we can then look for the words most specific to each inaugural speech. 
## Could be quantified by calculating the tf-idf (term frequency-inverse document frequency) 
## of each term-speech pair using the bind_tf_idf() - see CH3

inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

## this data could be used to pick four notable inaugural address (Lincoln, Roosevelt, Kennedy, Obama)
## and visualize the words most specific to each speech

notable_speeches <-  c("1861-Lincoln", "1933-Roosevelt", "1961-Kennedy", "2009-Obama")

## getting closer, I need to look at some of the older code. Like how to just 
## get the top 10 words for each group.

## Let's walk through it.
## I want only 4 speeches (see notable_speeches)
## I want to plot the top 10 tf_idf terms for each speech in a subplot 
inaug_tf_idf %>%
  filter(document %in% notable_speeches) %>%
  arrange(desc(tf_idf)) %>%
  group_by(document) %>%
  slice(1:10)


inaug_tf_idf %>%
  filter(document %in% notable_speeches) %>%
  arrange(desc(tf_idf)) %>%
  group_by(document) %>%
  slice(1:10) %>%
  ggplot(aes(tf_idf, term, fill=document)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ document)
  
## For some reason the above still includes the words with 0 tf_idf (that don't occur in that particular speech)
## Need to have scales = "free" in the facet wrap!
inaug_tf_idf %>%
  filter(document %in% notable_speeches) %>%
  arrange(desc(tf_idf)) %>%
  group_by(document) %>%
  slice(1:10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, term, fill=document)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ document, scales = "free")

## Got it!

## We can also extract the year from each document's name
## and compute the total number of words within each year.
## tidyr's complete() function included zeroes (cases where a word didn't appear in the document)

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

## terms will include punctuation
## Pick several words and visualize how they changed in frequency over time

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count/ year_total)) +
  geom_point() + 
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "% frequency of word in inaugural address")

# 5.2 Casting tidy text data into a matrix

## Some algorithms expect DTMs as input
## We can use tidytext's cast_* verbs for converting from a tidy form to these matrices

## Take the tidied AP dataset and cast it back into a DTM using cast_dtm()

ap_td %>%
  cast_dtm(document, term, count)

## Could also cast the table into a dfm object from quanteda's dfm with cast_dfm()

ap_td %>%
  cast_dfm(document, term, count)


## Some tools simply require a sparse matrix

## cast the tidied AP data into a matrix object
library(Matrix)

m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)
dim(m)

## Create a DTM of the Jane Austen corpus

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>% # create a tidy dataset
  count(book, word) %>% # count the number of times a word occurs in a book
  cast_dtm(book, word, n) # "cast" the tidy Jane Austen data into a DTM

austen_dtm

# 5.3 Tidying corpus objects with metadata

