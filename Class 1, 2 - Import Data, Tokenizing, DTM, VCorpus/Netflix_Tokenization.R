#######################
# 1. Loading Unstructured Data
#######################
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles

library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)

######################################
# 2. Rename Columns to industry standard
######################################

colnames(netflix)[12] <- "text"

# netflix %>% 
#   rename(text = description) %>% 
#   glimpse()

# Looking at location information:
unique(netflix$country)

######################################
# 3. Tokenization and Counting
#####################################
tidy_netflix <- netflix %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(country, word, sort=TRUE)

tidy_netflix_dtm <- tidy_netflix %>% 
  cast_dtm(country, word, n)
  


