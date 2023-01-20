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
library(tidyr)

## Date: 18 January 2023
## Testing out regex with the VIP Fall 2022 news articles

## First, load all of the pdf news articles into a list of pdf files

files <- list.files(pattern = "pdf$")
files
length(files)

## Use lapply to to extract the text from all of the pdf files in the list 
## In this case the files are all in the working directory
vip <- lapply(files, pdf_text) 

lapply(vip, length)
length(vip)

# Have a look at one of the documents
vip[[1]]
length(vip[[1]])
## The length of each files corresponds to the number of pdf pages

## Want to remove the first 2 lines of text which contains the title, newspaper, and page number
## Want to remove the line line of text which has the copyright info
for (x in 1:length(vip)){
  #print(x)
  vip[[x]] <- str_replace(vip[[x]], regex("[a-zA-Z0-9_].*[\n]"), "")
  vip[[x]] <- str_replace(vip[[x]], regex("[a-zA-Z0-9_].*[\n]"), "")
  vip[[x]] <- str_replace(vip[[x]], regex("Copyright.*[\n]"), "")
}

#vip[[1]]
#vip[[2]]
#vip[[3]]
#vip[[42]]
## Next, build the corpus using the Corpus() function from the tm package
vip_corp <- Corpus(VectorSource(vip))
vip_corp  # I don't like this because I lose the document names, but maybe I could match them somehow, like with files.

#corp <- Corpus(URISource(files), # First argument is what we want to use to create the corpus, the vector of pdf files. URI (uniform resource identifier) source means that the vector of file names identifies our resources
 #              readerControl = list(reader = readPDF)) # Second argument requires a list of control parameters, one is reader, to read in PDF files, same in tm as pdftools package

#corp

## From this corpus, create a document term matrix.
## The DTM will be useful for topic modeling but can be converted into a tidy data frame 
# for descriptive and exploratory analysis

vip.dtm <- DocumentTermMatrix(vip_corp, #change to vip_corp for testing
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

## Make Document a Number

vip.tidy$document <- as.numeric(vip.tidy$document)
vip.tidy

## Replace the document number with the document name from the list file

for (x in 1:length(files)) {
  print(files[x])
  vip.tidy$document[vip.tidy$document == x] <- files[x]
}
vip.tidy

## From the document column parse out the article name (first set of characters before __), 
## the newspaper (second set of character between __ and ___), and the date (third set of characters between ___ and __)

#str_match(vip.tidy$document, regex("(.*?)(?=__)")) # This gets the article title

#str_match(vip.tidy$document, regex("(?<=__)[a-zA-Z]+-?_?.*(?=___)")) # This gets the newspaper name

#str_match(vip.tidy$document, "[a-zA-Z]+_[0-9]+_[0-9]+(?=__)") # This gets the date

vip.tidy <- vip.tidy %>%
  mutate(title = str_extract(document, regex("(.*?)(?=__)"))) %>%
  mutate(newspaper = str_extract(document, regex("(?<=__)[a-zA-Z]+-?_?.*(?=___)"))) %>%
  mutate(date = str_extract(document, "[a-zA-Z]+_[0-9]+_[0-9]+(?=__)"))
vip.tidy


## What are the most common words in the corpus?

vip.tidy %>%
  count(term, sort = TRUE)

vip.tidy %>%
  count(term, sort = TRUE) %>%
  filter(n > 10) %>% 
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term)) +
  geom_col() +
  labs(y = NULL)


## I think I may have done this correctly. But I need to match the document name back to the tidy df. 
## Then I want to figure out how to break out the document name to different parts like title, newspaper, date, and animal.

## Sentiment Analysis

vip.sent <- vip.tidy %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
  count(document, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(vip.sent, aes(document, sentiment, fill = sentiment)) + 
  geom_col(show.legend = FALSE)

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


## I want to move on to testing the LDA. Can think more about text mining later.

## Here we want to use the vip.dtm to run the LDA
## Just two topics for now to see if it can find the bear and wolf articles
vip.dtm2 <- vip.tidy %>%
  cast_dtm(document, term, count) # recast, not sure why, but otherwise I get a funny error

vip.lda2 <- LDA(vip.dtm2, k = 2, control = list(seed = 1234))
vip.lda2

vip.topics2 <- tidy(vip.lda2, matrix = "beta")
vip.topics2

vip.top.terms2 <- vip.topics2 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta) # arrange sorts the data

vip.top.terms2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered()

# Let's do 16 topics. The number of codes in the codebook

vip.lda16 <- LDA(vip.dtm2, k = 16, control = list(seed = 1234))
vip.lda16

vip.topics16 <- tidy(vip.lda16, matrix = "beta")
vip.topics16

vip.top.terms16 <- vip.topics16 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% # Some have more than 10 because the beta values are "tied"
  ungroup() %>%
  arrange(topic, -beta) # arrange sorts the data

vip.top.terms16 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered()

vip.docs16 <- tidy(vip.lda16, matrix = "gamma")
vip.docs16

vip.docs16 %>%
  mutate(title = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))

vip.docs16.classification <- vip.docs16 %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup()
vip.docs16.classification

vip.docs16.classification %>%
  count(topic) %>%
  ggplot(aes(topic, n, fill=n)) + 
  geom_bar(stat="identity")




