######################################################
########Step1: building a small text object###########
######################################################

my_txt <- c("I think that data is the new bacon",
            "what is more. i think text analytics is the new turkey bacon",
            "very few people know how good Canadia bacon is",
            "and even fewer people know how a beaver tail tastes like",
            "Putin fench fries are now so good")

######################################################
###STEP2: Putting the vector in a data frame##########
######################################################

#install.packages("dplyr")
library(dplyr)
mydf <- data.frame(line=1:5, text=my_txt)
print(mydf)

######################################################
######## Step3: tokenizing the mydf dataframe#########
######################################################
#install.packages("tidytext")
#install.packages("tidyverse")
library(tidytext)
library(tidyverse)
token_list <- mydf %>%
                  unnest_tokens(word, text)
                        #no punctutation, no upper case letters
print(token_list)

#######################################################
##########STEP4: token frequencies####################
#######################################################

frequencies_tokens <- mydf %>%
                        unnest_tokens(word, text) %>%
                          count(word, sort=TRUE)
print(frequencies_tokens)

#######################################################
#########STEP5:### stop words #########################
#######################################################

#stop words are words that are commonly used in English 
# e.g. is, I, are, you, me, the, of, etc.
#we will use the anti_join(stop_words) to remove the stop words
library(dplyr)
library(stringr)
library(tidytext)

data(stop_words)
frequencies_tokens_nostop <- mydf %>%
                        unnest_tokens(word, text) %>%
                        anti_join(stop_words) %>% #here's where we remove tokens
                        count(word, sort=TRUE)
                        

print(frequencies_tokens_nostop)

#######################################################
#####STEP6:  token frequency histograms################
#######################################################

library(ggplot2)
freq_hist <- mydf %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                count(word, sort=TRUE) %>%
                mutate(word=reorder(word, n)) %>%
                ggplot(aes(word, n))+
                geom_col()+
                xlab(NULL)+
                coord_flip()
print(freq_hist)


