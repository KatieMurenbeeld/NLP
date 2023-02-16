library(httr) # for accessing the html from the link
library(stringr) # for extracting the relevant text
library(tidyverse)
library(tidytext) # for creating a tidy dataframe from the website text data
library(dplyr) 

# set your working directory
#setwd()

# read in the csv with the urls for the articles
csv <- read.csv(file = 'articles_2.csv')

# check the column headers and make an article ID column

# create a vector or list of the urls from the csv
urls <- csv$Link

#for (url in urls){
#  print(url)}

# for each url:
# 1) use httr's GET() function to request the url
# 2) use httr's content() function access the body of the request as text
# 3) use stringr's str_extract_all() function to extract the text between the <p></p> 
# <p></p> is the html paragraph element and contains the text of interest for the article
# make sure to set simplify = TRUE
# 4) #3 will result in a 3 column table. We want the second column

urls_test <- urls[1:10]

df_article <- data.frame()


for (url in urls_test){
  link <- url
  page <- GET(link)
  page_text <- content(page, as = 'text')
  text_body <- str_extract_all(page_text, regex("(?<=<p>).+(?=</p>)"), simplify = TRUE) 
  text <- text_body[[2]]
  df_article <- rbind(df_article, text)
}

colnames(df_article)<-c("article.text")
df_article <- df_article %>% mutate(id = row_number())



#### Code testing below this comment #####

url <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica&sort=YMD_date%3AD&page=17&fld-base-0=alltext&maxresults=20&val-base-0=%22grizzly%20bears%22&docref=news/18CE672FEF7A3C60"
url2 <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica/country%3AUSA%21USA&sort=YMD_date%3AD&page=2&maxresults=20&f=advanced&val-base-0=%22grizzly%20bears%22&fld-base-0=alltext&docref=news/18D2BD9E847AFCE8"
url3 <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=&sort=YMD_date%3AD&fld-base-0=alltext&maxresults=20&val-base-0=Citing%20grizzlies%2C%20groups%20sue%20over%20grazing%20plan%20in%20Paradise%20Valley&docref=news/18C8199AA9C17BD0"

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
text_df <- tibble(text = body)
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



