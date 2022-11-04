# From "Text Mining with R: A Tidy Approach" by Julia Silge & David Robinson



## Load the appropriate libraries
library(dplyr) # to transform data into "tidy" data
library(tidytext) # for text mining
library(janeaustenr) # text from Jane Austen's 6 completed, published novels
library(stringr) # makes working with strings easier
library(ggplot2) # for data visualization
library(scales)
library(tidyr)

#library(gutenbergr) # provides access to the public domain works of the Gutenberg Project
# gutenbergr not available for my version of R, was removed from CRAN repo

## Create a variable called text with 4 strings (Emily Dickinson)

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
## Put the data into a data frame using the tibble() function

text_df <- tibble(line = 1:4, text = text)

# Tokenize the data using tidytext's unnest_tokens() function
# unnest_tokens() will tokenize the data frame and tranform it into 
# a tidy data structure

text_df %>%
  unnest_tokens(word, text)

# word is the output column name that will be created
# text is the input column name (here piped from column "text" from text_df)

# Load the Jane Austen data
# group by book
# annotate a linenumber quatitiy (data in a one-row-per-line format)
# annotate a chapter (using a regex) to find where all the chapters are
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

# Want this to be in a one-token-per-row format (tidy)

tidy_books <- original_books %>%
  unnest_tokens(word, text)


# Now that the text is tidy we can manipulate it with tidy tools
# Below we will remove "stop words" using the anti_join() function

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

# Find the most common words in all the books

tidy_books %>%
  count(word, sort = TRUE)

# Now we can visualize the most common words using ggplot

tidy_books %>%
  count(word, sort=TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# Let's get the word frequency for different Jane Austen novels

tidy_books %>%
  filter(book == "Sense & Sensibility") %>%
  count(word, sort = TRUE)

filter(tidy_books, book == "Emma") %>%
  count(word, sort = TRUE)

# Calculate the frequency of words from three of the Jane Austen novels
# could probably do some sort of loop to do this for all 6 books

frequency <- bind_rows(mutate(tidy_books, book = "Sense & Sensibility"),
                       mutate(tidy_books, book = "Emma"),
                       mutate(tidy_books, book = "Pride & Prejudice")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(book, word) %>%
  group_by(book) %>%
  mutate(proportion = n /sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = book, values_from = proportion) %>%
  pivot_longer('Sense & Sensibility':'Emma',
               names_to = "book", values_to = "proportion")

# Let's plot!

ggplot(frequency, aes(x = proportion, y = `Pride & Prejudice`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~book, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Pride & Prejudice", x = NULL)


# Let's try again...

ss <- filter(tidy_books, book == "Sense & Sensibility") 
emma <- filter(tidy_books, book == "Emma") 
pp <- filter(tidy_books, book == "Pride & Prejudice") 

frequency2 <- bind_rows(mutate(ss, author = "Jane Austen"),
                       mutate(emma, author = "Jane A"),
                       mutate(pp, author = "J Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n /sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer('Jane Austen':'Jane A',
               names_to = "author", values_to = "proportion")


ggplot(frequency2, aes(x = proportion, y = `J Austen`, 
                      color = abs(`J Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "J Austen", x = NULL)

# In this case 
# J Austen = Pride & Prejudice
# Jane A = Emma
# Jane Austen = Sense & Sensibility

# Now we can quantify how similar and different the words frequencies are
# using Pearson correlation

# How correlated are the word frequencies between Pride & Prejudice and Emma?
# How correlated are the word frequencies between Pride & Prejudice and Sense & Sensibility?
cor.test(data = frequency2[frequency2$author == "Jane A",],
         ~ proportion + `J Austen`)
cor.test(data = frequency2[frequency2$author == "Jane Austen",],
         ~ proportion + `J Austen`)


# word frequencies are more correlated between Pride & Prejudice and Sense & Sensibility
# than between Prode & Prejudice and Emma.



