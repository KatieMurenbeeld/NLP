library(tidyverse)
#library(rvest)
#library(jsonlite)
library(stringr)
library(xml2)
#library(RSelenium)
library(httr)

url <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica&sort=YMD_date%3AD&page=17&fld-base-0=alltext&maxresults=20&val-base-0=%22grizzly%20bears%22&docref=news/18CE672FEF7A3C60"

page <- GET(url)
# You need to be connected to the VPN, otherwise you get text of the javascript telling you to log in. 
page_text <- content(page, as = 'text')
page_text
lengths(regmatches(page_text, gregexpr("<p>", page_text)))
grepl('document-view__body read document-view__body--ascii ', page_text, ignore.case=T) # this is just a check.
# when you look at the source code on the website, this is the division class that contains the actual article text
text_body <- str_extract_all(page_text, regex("(?<=<p>).+(?=</p>)")) 
text_body

## From here I've been trying to clean up the text more, but having a bit of trouble
## Since the urls all live within a column of the article_coding.csv, I imagine a script that loops through each url would be easy
## to make, like you suggested before. 

#text_body <- str_replace_all(text_body, "<br/><br/>" , " ")
text_body <- str_replace_all(text_body, "[\r\n\t\f\v]" , " ")
text_body
text_body <- str_replace_all(text_body, "<br/><br/>" , " ")
text_body <- str_replace_all(text_body, "<stong>" , " ") #not sure why these don't come out?
text_body <- str_replace_all(text_body, "</stong>" , " ") #not sure why these don't come out?
text_body
text_body <- str_replace_all(text_body, "(?<=).+(?=</p>)" , " ")
text_body



