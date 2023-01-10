library(RCurl)
library(XML)
library(stringr)
library(plyr)

# Date: 10 January 2023
# Learning how to scrape the web 
# Tutorial from here: https://www.scrapingbee.com/blog/web-scraping-r/


## Parsing a webpage using R

#scrape_url <- "https://www.google.com/"
scrape_url <- "https://www.york.ac.uk/teaching/cws/wws/webpage1.html"

flat_html <- readLines(con = scrape_url)

## Using R to download files over FTP

ftp_url <- "ftp://cran.r-project.org/pub/R/web/packages/BayesMixSurv/"

get_files <- getURL(ftp_url, dirlistonly = TRUE) # Need RCurl package for getURL

# Can use str_split() from the stringer package to remove the DOS line endings
extracted_filenames <- str_split(get_files, "\r\n")[[1]] # Need stringr package for str_split()

# get just the html files
extracted_html_filenames <- unlist(str_extract_all(extracted_filenames, ".+(.html)"))


# Create a function to download files with getURL() and save it to a local folder

FTPDownloader <- function(filename, folder, handle) {
  
  dir.create(folder, showWarnings = FALSE)
  fileurl <- str_c(ftp_url, filename)
  
  if (!file.exists(str_c(folder, "/", filename))) {
    file_name <- try(getURL(fileurl, curl = handle))
    write(file_name, str_c(folder, "/", filename))
    
    Sys.sleep(1)
    
  }
}

# Need a cURL handle for the actual network communication
Curlhandle <- getCurlHandle(ftp.use.epsv = FALSE)

# From the plyr package (usefule for splitting data, analyzing, and putting it back together)
# plyr is an earlier iteration of dplyr

l_ply(extracted_html_filenames, FTPDownloader, folder = "scrapingbee_html", handle = Curlhandle)

## Scraping information from Wikipedia using R

# Here are the basic steps to parse information

# 1. Save the URL is wiki_url
wiki_url <- "https://en.wikipedia.org/wiki/Leonardo_da_Vinci"

# 2. Fetch the HTML content of the URL and save it in wiki_read
wiki_read <- readLines(wiki_url, encoding = "UTF-8")

# 3. Parse the HTML code into a DOM (Document Object Model, a typed, in-memory representation of an HTML doc)
# and save as parsed_wiki
parsed_wiki <- htmlParse(wiki_read, encoding = "UTF-8") # Need XML package

# now we can get a list of <p> (paragraph) elements from the parsed_wiki
# wiki_intro_text will contain a list of all paragraphs

wiki_intro_text <- parsed_wiki["//p"]

# Access the fourth element in the list of paragraphs

wiki_intro_text[[4]]

# Get a list of all the links in the page and check the number of links in the page

getHTMLLinks(wiki_read)
length(getHTMLLinks(wiki_read))

# Can also scrape tables off HTML pages
# Save a new url as wiki_url1
wiki_url1 <- "https://en.wikipedia.org/wiki/Help:Table"

# Fetch the HTML content of the URL and save it in wiki_read1
wiki_read1 <- readLines(wiki_url1, encoding = "UTF-8")

# How many tables are in the page?
length(readHTMLTable(wiki_read1))

# Choose table. Get the table names using names()
names(readHTMLTable(wiki_read1))

# A lot of NULLs but let's look at "Basic table\n"
readHTMLTable(wiki_read1)$"Basic table\n"


## I want to test out an article from the VIP class at this point. 

# Save the url as vip_url
vip_url <- "https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica/country%3AUSA%21USA&sort=YMD_date%3AD&page=1&maxresults=20&f=advanced&val-base-0=%22grizzly%20bears%22&fld-base-0=alltext&docref=news/18D370A541F74FA8https://infoweb.newsbank.com/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica/country%3AUSA%21USA&sort=YMD_date%3AD&page=1&maxresults=20&f=advanced&val-base-0=%22grizzly%20bears%22&fld-base-0=alltext&docref=news/18D370A541F74FA8"

# Fetch the HTML content of vip_url and save in vip_read
vip_read <- readLines(vip_url, encoding = "UTF-8")

# Parse the HTML into a DOM and save as vip_parsed
vip_parsed <- htmlParse(vip_read, encoding = "UTF-8")

vip_parsed # The infoweb.newsbank doesn't really seem like a real article, 
# But I can download them as pdfs. Here is a helpful link for converting 
# pdfs for NLP in R https://ladal.edu.au/pdf2txt.html

# Get a list of the paragraph <p> elements
vip_p_text <- vip_parsed["//p"]

# What is the first element in the list of paragraphs?
vip_p_text

# Let's try this with a regular (not infoweb) link

vip_url1 <- "https://helenair.com/news/state-and-regional/govt-and-politics/citing-grizzlies-groups-sue-over-grazing-plan-in-paradise-valley/article_15e4349b-5435-5a91-ac97-a24aee8a539b.html"
vip_read1 <- readLines(vip_url1, encoding = "UTF-8")
vip_parsed1 <- htmlParse(vip_read1, encoding = "UTF-8")
vip_p_text1 <- vip_parsed1["//p"]
vip_p_text1

length(vip_parsed1["//title"])
# That worked ok. I guess from there you can remove more of the strings.
vip_parsed1["//title"]

## Let's move on to the rvest package and see if it is more helpful

library(rvest)

vip_gb <- read_html("https://helenair.com/news/state-and-regional/govt-and-politics/citing-grizzlies-groups-sue-over-grazing-plan-in-paradise-valley/article_15e4349b-5435-5a91-ac97-a24aee8a539b.html",
                    encoding = "UTF-8")

vip_gb %>% html_elements("title")
vip_gb %>% html_elements("p")

# The above results in lists of paragraphs,
# so how can I combine them all into one long string
# Create two lists
list1 = list('sai','ram','deepika','sahithi')
list2 = list('kumar','scott','Don','Lin')
# Using c()
list3 <- c(list1,list2)
print(list3)
list1.2 <- paste(list1, collapse = " ")
list1.2
list2.2 <- paste(list2, collapse = " ")
list4 <- paste(list1.2, list2.2, collapse = " ")
list4

vip_gb_p <- vip_gb %>% html_elements("p")
vip_gb_p
vip_gb_all_p <- paste(vip_gb_p, collapse = " ")
vip_gb_all_p


extracted_vip_bg_p <- str_replace_all(vip_gb_all_p, "<p>", "")
extracted_vip_bg_p


extracted_vip_bg_p_test <- str_replace_all(vip_gb_all_p, 
                                           c('<p>' = '', 
                                             "</p>" = "",
                                             "<em>" = "",
                                             "</em>" = "",
                                             "[\r\n]" = ""
                                             ))
extracted_vip_bg_p_test
extracted_vip_bg_p_test2 <- gsub("<[^>]+>", "", vip_gb_all_p)
extracted_vip_bg_p_test2
#extracted_vip_bg_p_test2 <- gsub("{{}}", "", extracted_vip_bg_p_test2)
extracted_vip_bg_p_test2 <- str_replace_all(extracted_vip_bg_p_test2,
                                            "[\r\n]", "")
extracted_vip_bg_p_test2

extracted_vip_bg_p_test2 <- str_replace_all(extracted_vip_bg_p_test2, 
                                           "[[:punct:]]","")
                                           
extracted_vip_bg_p_test2
# I do think I'll be able to figure this out. I need to go back through 
# the first few chapters of the Text Mining with R.

library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)

text_vip <- tibble(text = extracted_vip_bg_p_test2)

text_vip
tidy_vip <- text_vip %>% 
  unnest_tokens(word, text)

data(stop_words)
tidy_vip <- tidy_vip %>%
  anti_join(stop_words)

stop_words
tidy_vip

tidy_vip %>%
  count(word, sort = TRUE)

tidy_vip %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) + 
  geom_col() + 
  labs(y = NULL)

tidy_vip %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20))

get_sentiments("bing")

bing_word_counts_vip <- tidy_vip %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts_vip %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(x = "Contribution to sentiment",
       y = NULL)

## what about getting a pdf? https://ladal.edu.au/pdf2txt.html or https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

library(pdftools)

files <- list.files(pattern = "pdf$")
files

vip <- lapply(files, pdf_text) 

lapply(vip, length)

library(tm)

corp <- Corpus(URISource(files), 
               readerControl = list(reader = readPDF))

vip.tdm <- TermDocumentMatrix(corp,
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = FALSE,
                                          tolower = TRUE,
                                          stemming = FALSE,
                                          removeNumbers = FALSE,
                                          bounds = list(global = c(3, Inf))))

inspect(vip.tdm[1:10,])

vip.tidy <- tidy(vip.tdm)
vip.tidy

vip_sentiments <- vip.tidy %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

vip_bing_wc <- vip_sentiments %>%
  count(term, sentiment, sort = TRUE) %>%
  ungroup()

vip_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 2) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() +
  labs(x = "Contribution to sentiment", y = NULL)

# Term frequency

#vip.words <- vip.tidy %>%
#  count(document, term, sort = TRUE)
#vip.words

vip.total.words <- vip.words %>%
  group_by(document) %>%
  summarise(total = sum(n))
vip.total.words

vip.words <- left_join(vip.tidy, vip.total.words)
vip.words 

ggplot(vip.words, aes(count/total, fill = document)) + 
  geom_histogram(bins = 10, show.legend = FALSE) + 
  xlim(NA, 0.0009) + 
  facet_wrap(~document, ncol = 2, scales = "free_y")

# I think I'm having issues because count is a double and total is an integer?


