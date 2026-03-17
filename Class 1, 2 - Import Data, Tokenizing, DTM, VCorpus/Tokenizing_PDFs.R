################################
# 1. Create a vector of strings
################################
my_txt <- c("I think that data is the new bacon",
            "What is more, I think text analytics is the new turkey bacon",
            "very few people know how good Canadian bacon is",
            "and even fewer people know how a beaver tail tastes",
            "Poutine french fries are also really good.")

###############################
# 2. Putting this vector into a data frame
###############################
library(dplyr)
mydf <- data_frame(text=my_txt, line=1:5)
print(mydf)

###############################
# 3. Tokenize
###############################
library(tidyverse)
library(tidytext)
library(stringr)

token_list <- mydf %>% 
  unnest_tokens(word, text) %>%   # tokenize values
  anti_join(stop_words) %>%       # remove stop words
  count(word, sort=TRUE) %>%      # count the words
  arrange(n)                      # sort in ascending order (hidden business value)