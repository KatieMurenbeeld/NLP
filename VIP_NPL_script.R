# For the first part of the script (the web scraping) only load these libraries

library(httr) # for accessing the html from the link
library(stringr) # for extracting the relevant text
library(dplyr) # package to help manipulate data frames


# set your working directory
#setwd()

# read in the csv with the urls for the articles
# article_codes <- read.csv(file = 'articles_2.csv')
article_codes <- read.csv(file = 'article_coding_20230221.csv')

# check the column headers and make an article ID column
article_codes <- article_codes %>% 
  filter(!Link=='') %>% 
  mutate(id = row_number())

# create a vector or list of the urls from the csv
urls <- article_codes$Link

# for each url:
# 1) use httr's GET() function to request the url
# 2) use httr's content() function access the body of the request as text
# 3) use stringr's str_extract_all() function to extract the text between the <p></p> 
# <p></p> is the html paragraph element and contains the text of interest for the article
# make sure to set simplify = TRUE
# 4) #3 will result in a 3 column table. We want the second column

# Just for testing the for loop
# urls_test <- urls[1:10]

# Create an empty dataframe 
df_article <- data.frame()


# Must be connected to VPN or on campus for this to work.

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
colnames(df_article)<-c("article.text")
df_article <- df_article %>% mutate(id = row_number()) %>% select(id, article.text)

# from the article_codes data frame select one or more variables/columns you would like to join to df_article
# you can update the columns in select to include whatever information you'd like
# for example, article type, newspaper, etc. But you need the "id" column so that you can "join" the 2 dataframes
df_to_join <- article_codes %>% select(id, Species) 
df_article <- df_article %>% full_join(df_to_join, by = "id")

# Create a tidy data frame and create some plots!

# Load the rest of the libraries (httr doesn't seem to like it when all of these are loaded)
library(tidyverse) # an "opinionated" collection of packages. Includes, dplyr, ggplot2, forcats, tibble, readr, stringr, tidyr, and purr
library(tidytext) # for creating a tidy dataframe from the website text data
library(ggplot2) # package for plotting
library(wordcloud) # package for creating word clouds
library(reshape2) # package to restructure and aggregate data
library(tm) # package for topic modeling
library(topicmodels) # package for topic modeling

# Make a tidy data frame by "unnesting" the tokens
# This automatically remove punctuation and lowercase all words
tidy_df <- df_article %>% unnest_tokens(word, article.text)

# Remove "stop words" from the SMART lexicon 
data("stop_words")
tidy_df <- tidy_df %>%
  anti_join(stop_words)

# Create a small data frame of stop words for this project ("br" and "strong" which are part of the html formatting which should come out before making tidy df)
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
  cast_dtm(word, id, n)

# Can create separate dtms for each species by filtering for species before generating the dtm
dtm_bears <- tidy_df %>% 
  filter(Species == "Grizzly Bear" | Species == "Grizzly bear") %>%
  count(id, word) %>%
  cast_dtm(word, id, n)
dtm_beavers <- tidy_df %>% 
  filter(Species == "Beavers" | Species == "beavers") %>%
  count(id, word) %>%
  cast_dtm(word, id, n)
dtm_boars <- tidy_df %>% 
  filter(Species == "Boars") %>%
  count(id, word) %>%
  cast_dtm(word, id, n)
dtm_wolves <- tidy_df %>% 
  filter(Species == "Wolves") %>%
  count(id, word) %>%
  cast_dtm(word, id, n)


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

# Can plot for the different species in 2 ways. Can filter() for the species or 
# use a pre-filtered data frames (see lines 89-96)
# Option 1:
#tidy_df %>% filter(Species == "Beavers") %>%

# Option 2: 
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
  labs(title = "20 Most Common Words in Articles About Boars", # update the Species
       y = NULL)

# To plot the most common positive and negative words 

# If you want to do this for specific species...
# You will always need to create a a data frame of sentiment word counts.

# Create data frame of sentiment word counts
vip.bing.counts <- tidy_df %>%
  filter(Species == "Boars") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Use the sentiment word counts data frame to make the plot
vip.bing.counts %>%
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

# Word clouds
# Can do for each animal or for the entire corpus
tidy_df_boars %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


## Modeling 




