library(tidyverse)
library(tidytext)
library(dplyr)
library(rvest)
#library(jsonlite)
library(stringr)
library(xml2)
#library(RSelenium)
library(httr)

url <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica&sort=YMD_date%3AD&page=17&fld-base-0=alltext&maxresults=20&val-base-0=%22grizzly%20bears%22&docref=news/18CE672FEF7A3C60"
url2 <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica/country%3AUSA%21USA&sort=YMD_date%3AD&page=2&maxresults=20&f=advanced&val-base-0=%22grizzly%20bears%22&fld-base-0=alltext&docref=news/18D2BD9E847AFCE8"
  
page <- GET(url)
page2 <- GET(url2)
# You need to be connected to the VPN, otherwise you get text of the javascript telling you to log in. 
page_text <- content(page, as = 'text') # also seems to work with as = 'parsed'
page_text
page_text2 <- content(page2, as = 'text')
page_text2
lengths(regmatches(page_text, gregexpr("<p>", page_text)))
lengths(regmatches(page_text2, gregexpr("<p>", page_text2)))
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

test_body
text_df <- tibble(text = body)
text_df %>% unnest_tokens(word, text)


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



