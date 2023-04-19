# load packages
library(tidyverse)
library(rvest)
library(readtext)
library(flextable)
library(webdriver)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# tutorial and rmd from https://ladal.edu.au/webcrawling.html 

# example of search url for "feral hogs", only web sources, and only in North America:
# first page of results
# https://infoweb.newsbank.com/apps/news/results?p=WORLDNEWS&t=stp%3AWeb-Only%2BSource%21Web-Only%2BSource/continent%3ANorth%2BAmerica%21North%2BAmerica&sort=YMD_date%3AD&fld-base-0=alltext&maxresults=20&val-base-0=%22feral%20hog%22&f=advanced
# subsequent pages
# https://infoweb.newsbank.com/apps/news/results?page=1&p=WORLDNEWS&t=stp%3AWeb-Only%2BSource%21Web-Only%2BSource/continent%3ANorth%2BAmerica%21North%2BAmerica&sort=YMD_date%3AD&fld-base-0=alltext&maxresults=20&val-base-0=%22feral%20hog%22&f=advanced

# Phantom JS
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


# Testing

url <- "https://infoweb.newsbank.com/apps/news/results?p=WORLDNEWS&t=stp%3AWeb-Only%2BSource%21Web-Only%2BSource/continent%3ANorth%2BAmerica%21North%2BAmerica&sort=YMD_date%3AD&fld-base-0=alltext&maxresults=20&val-base-0=%22feral%20hog%22&f=advanced"
#url <- "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C13&q=feral+hog&btnG="
#url <- "https://www.theguardian.com/world/angela-merkel"
pjs_session$go(url)
# render page
rendered_source <- pjs_session$getSource()
# download text and parse the source code into an XML object
html_document <- read_html(rendered_source)

# For theguardian url
#links <- html_document %>%
#  html_nodes(xpath = "//div[contains(@class, 'fc-item__container')]/a") %>%
#  html_attr(name = "href")

# For the infoweb url
links <- html_document %>%
  html_nodes(xpath = "//div[contains(@class, 'search-hits')]/a") %>%
  html_attr(name = "href")

# Need to add the "https://infoweb.newsbank.com" to the front 
links <- paste0("https://infoweb.newsbank.com", links)

links

test_url <- links[[4]]

pjs_session$go(test_url)
rendered_source <- pjs_session$getSource()
test_document <- read_html(rendered_source)

title <- test_document %>%
  rvest::html_node("h1") %>%
  rvest::html_text(trim = T)

text <- test_document %>%
  rvest::html_nodes("p") %>%
  #rvest::html_attr("p") %>%
  rvest::html_text(trim = T)

# Date is from <span class="display-date">April 3, 2023</span>
date <- test_document %>%
  rvest::html_nodes("span[class='display-date']") %>%
  rvest::html_text(trim = T)

# Grab the source too
source <- test_document %>%
  rvest::html_nodes("span[class='source']") %>%
  rvest::html_text(trim = T)

article <- data.frame(
  url = url,
  date = date,
  title = title,
  source = source,
  body = text[[9]])

# A function to get the title, date, and text from the links
# Function updated from https://ladal.edu.au/webcrawling.html 
scrape_article <- function(url) {
  # start PhantomJS
  pjs_session$go(url)
  rendered_source <- pjs_session$getSource()
  # read raw html
  html_document <- read_html(rendered_source)
  # extract title
  title <- html_document %>%
    rvest::html_node("h1") %>%
    #rvest::html_nodes("h1 [class='document-view__title read clipcopy']") %>%
    rvest::html_text(trim = T)
  # extract text
  text <- html_document %>%
    rvest::html_nodes("p") %>%
    rvest::html_text(trim = T)
  # extract date
  date <- html_document %>%
    rvest::html_nodes("span[class='display-date']") %>%
    rvest::html_text(trim = T)
  # extract source
  source <- html_document %>%
    rvest::html_nodes("span[class='source']") %>%
    rvest::html_text(trim = T)
  # generate data frame from results
  article <- data.frame(
    url = url,
    date = date,
    title = title,
    source = source,
    body = text[[9]]
    )
  
  return(article)
  
}

test_article <- scrape_article(links[[4]])

# create container for loop output
all_articles <- data.frame()
# loop over links
for (i in 1:length(links)) {
  # print progress (optional)
  #cat("Downloading", i, "of", length(all_links), "URL:", all_links[i], "\n")
  # scrape website
  article <- scrape_article(links[i])
  # append current article data.frame to the data.frame of all articles
  all_articles <- rbind(all_articles, article)
}

## Super close to what I want. Need to think about the Title and Text a little more
## Some articles have more paragraphs than others because for some articles the 
## title is a p child in a h1 parent