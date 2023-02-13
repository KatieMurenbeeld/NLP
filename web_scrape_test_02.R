library(tidyverse)
library(rvest)
library(jsonlite)
library(stringr)
library(xml2)
library(RSelenium)
library(httr)

url <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica&sort=YMD_date%3AD&page=17&fld-base-0=alltext&maxresults=20&val-base-0=%22grizzly%20bears%22&docref=news/18CE672FEF7A3C60"

#url %>%
#  read_html()

#main_body <- "document-view__body read document-view__body--ascii "

#url_body <- url %>%
#  read_html() 

#url_body

#xml_child(url_body, 2)

#str_extract(url_body, regex("document-view__body read document-view__body--ascii ")) 


#json_as_text <- html %>% html_element("body") %>% html_text()

#document.querySelector("#document-view--ascii > div > div.document-view__content__inner > div.document-view__body.read.document-view__body--ascii")

#test <- xml_text(xml_child(url_body, 2))
#test
url2 = 
page <- GET(url)
page_text <- content(page, as = 'text')
page_text
lengths(regmatches(page_text, gregexpr("<p>", page_text)))
grepl('document-view__body read document-view__body--ascii ', page_text, ignore.case=T)
#str_extract(page_text, regex('(?<=<div class=\"document-view__body read document-view__body--ascii \">).'))
#str_extract_all(page_text, regex("(?<=<p>).+(?=</p>)"))
#str_replace_all(page_text, "<br/><br/>" , "")

#str_remove(page_text, regex("(?<=<p>).+(?=</p>)")) 
text_body <- str_extract_all(page_text, regex("(?<=<p>).+(?=</p>)")) 
#text_body <- str_replace_all(text_body, "<br/><br/>" , " ")
text_body <- str_replace_all(text_body, "[\r\n\t\f\v]" , " ")
text_body
text_body <- str_replace_all(text_body, "<br/><br/>" , " ")
text_body <- str_replace_all(text_body, "<stong>" , " ") #not sure why these don't come out?
text_body <- str_replace_all(text_body, "</stong>" , " ") #not sure why these don't come out?
text_body
text_body <- str_replace_all(text_body, "(?<=).+(?=</p>)" , " ")
text_body



