# From "Text Mining with R: A Tidy Approach" by Julia Silge & David Robinson



## Load the appropriate libraries
library(dplyr) # to transform data into "tidy" data
library(tidytext) # for text mining
library(janeaustenr) # text from Jane Austen's 6 completed, published novels
library(stringr) # makes working with strings easier
library(ggplot2) # for data visualization
library(ggraph) # extends ggplot2 to construct network plots
library(widyr) # calculates pairwise correlations and distances within a tidy data frame
library(tidyr)
library(forcats) # I think needed for fct_reorder
library(tm)
library(topicmodels)

# 6. 