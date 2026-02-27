######################################################
########Step1: building a small text object###########
######################################################

my_txt <- c(xxxx,
            xxxx,
            xxxx,
            xxxx,
            xxxx)

######################################################
###STEP2: Putting the vector in a data frame##########
######################################################

#install.packages("dplyr")
library(xxxx)
mydf <- xxxx(line=1:5, text=my_txt)
print(mydf)

######################################################
######## Step3: tokenizing the mydf dataframe#########
######################################################
#install.packages("tidytext")
#install.packages("tidyverse")
library(xxxx)
library(xxxx)
token_list <- mydf %>%
                  xxxx(word, xxxx)
                        #no punctutation, no upper case letters
print(token_list)

#######################################################
##########STEP4: token frequencies####################
#######################################################

frequencies_tokens <- mydf %>%
                        unnest_tokens(word, text) %>%
                          xxxx(xxxx, sort=TRUE)
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
                        xxxxx(xxxxx) %>% #here's where we remove tokens
                        count(word, sort=TRUE)
                        

print(frequencies_tokens_nostop)

#######################################################
#####STEP6:  token frequency histograms################
#######################################################

library(xxxxx)
freq_hist <- mydf %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                count(word, sort=TRUE) %>%
                xxxxx(word=reorder(word, n)) %>%
                xxxxx(aes(word, n))+
                geom_col()+
                xlab(NULL)+
                coord_flip()
print(xxxxx)


