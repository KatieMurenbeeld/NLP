# For the first part of the script (the web scraping) only load these libraries

library(httr) # for accessing the html from the link
library(stringr) # for extracting the relevant text
library(dplyr) # package to help manipulate data frames


# set your working directory
#setwd()

# read in the csv with the urls for the articles
article_codes <- read.csv(file = 'articles_2.csv')
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

tidy_df <- df_article %>% unnest_tokens(word, article.text)

# Remove "stop words"
data("stop_words")
tidy_df <- tidy_df %>%
  anti_join(stop_words)

# Create a small list of stop words for this project (br, strong which are part of the html formatting which should come out before making tidy df)
# You can add your own words to this list
my_stop_words <- data.frame(c("br", "strong")) 
colnames(my_stop_words) <-("word")

tidy_df <- tidy_df %>%
  anti_join(my_stop_words)

# Create a document term matrix. You will need this for topic modeling
#dtm <- tidy_df %>% cast_dtm(id, word)


## There is still a little bit of clean up to do for the text, but this is a better good start!
## Make sure to rename the csv as something more useful when you read it in and also add an ID column that way the two dataframes can join
## I guess I could also join on Link

tidy_df_bears <- tidy_df %>%
  filter(Species == "Grizzly Bear" | Species == "Grizzly bear")
tidy_df_beavers <- tidy_df %>%
  filter(Species == "Beavers" | Species == "beavers")
tidy_df_boars <- tidy_df %>%
  filter(Species == "Boars")
tidy_df_wolves <- tidy_df %>%
  filter(Species == "Wolves")

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








#### Code testing below this comment #####

url <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica&sort=YMD_date%3AD&page=17&fld-base-0=alltext&maxresults=20&val-base-0=%22grizzly%20bears%22&docref=news/18CE672FEF7A3C60"
url2 <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica/country%3AUSA%21USA&sort=YMD_date%3AD&page=2&maxresults=20&f=advanced&val-base-0=%22grizzly%20bears%22&fld-base-0=alltext&docref=news/18D2BD9E847AFCE8"
#url3 <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=&sort=YMD_date%3AD&fld-base-0=alltext&maxresults=20&val-base-0=Citing%20grizzlies%2C%20groups%20sue%20over%20grazing%20plan%20in%20Paradise%20Valley&docref=news/18C8199AA9C17BD0"
url3<- "http://www.columbia.edu/~fdc/sample.html"

page <- GET(url)
page2 <- GET(url2)
page3 <- GET(url3)
# You need to be connected to the VPN, otherwise you get text of the javascript telling you to log in. 
page_text <- content(page, as = 'text') # also seems to work with as = 'parsed'
page_text
page_text2 <- content(page2, as = 'text')
page_text2
page_text3 <- content(page3, as = 'text')
page_text3
lengths(regmatches(page_text, gregexpr("<p>", page_text)))
lengths(regmatches(page_text2, gregexpr("<p>", page_text2)))
lengths(regmatches(page_text3, gregexpr("<p>", page_text3)))
grepl('document-view__body read document-view__body--ascii ', page_text, ignore.case=T) # this is just a check.
# when you look at the source code on the website, this is the division class that contains the actual article text
grepl('document-view__body read document-view__body--ascii ', page_text2, ignore.case=T)
text_body <- str_extract_all(page_text, regex("(?<=<p>).+(?=</p>)"), simplify = TRUE) 
body <- text_body[[2]]
body
text_body2 <- str_extract_all(page_text2, regex("(?<=<p>).+(?=</p>)"), simplify = TRUE) 
text_body2
body2 <- text_body2[[2]]
body2
text_body3 <- str_extract_all(page_text3, regex("(?<=<p>).+(?=</p>)"), simplify = TRUE) 
text_body3
body3 <- text_body3[[2]]
body3

test_body
text_df <- tibble(text = body2)
text_df %>% add_row(text = body2)

tidy_test <- text_df %>% unnest_tokens(word, text)


## From here I've been trying to clean up the text more, but having a bit of trouble
## Since the urls all live within a column of the article_coding.csv, I imagine a script that loops through each url would be easy
## to make, like you suggested before. 

#text_body <- str_replace_all(text_body, "<br/><br/>" , " ")
text_body <- str_replace_all(text_body, "[\r\n\t\f\v]" , " ") # This changes it from Data to Values? 
text_body
text_df <- tibble(text_body = text)

text_body <- str_replace_all(text_body, "<br/><br/>" , " ")
text_body <- str_replace_all(text_body, "<stong>" , " ") #not sure why these don't come out?
text_body <- str_replace_all(text_body, "</stong>" , " ") #not sure why these don't come out?
text_body
text_body <- str_replace_all(text_body, "(?<=).+(?=</p>)" , " ")
text_body



