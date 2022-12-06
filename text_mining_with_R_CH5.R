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


