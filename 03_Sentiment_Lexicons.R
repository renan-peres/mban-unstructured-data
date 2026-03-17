# Sentiment in tidytext()
# install.packages("textdata")
library(textdata)
library(tidytext)
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

library(dplyr)
sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                          mutate(nrc, lexicon= "nrc"),
                          mutate(bing, lexicon="bing")
)

sentiments %>%
  filter(lexicon == "bing")

unique(sentiments$sentiment) #this is the qualitative
unique(sentiments$word)#this is the lexicon source
summary(sentiments$value) # this is 3rd score that we can use / quantitative

#########################################################
##### Lets take a look at the lexicons one by one #######
#########################################################
#how can we subset the data to get distinct lexicons?
# xxxxxxxx <- xxxxxxxxx
bing_sentiment <- subset(sentiments, lexicon == "bing")
filter(bing_sentiment, sentiment == "negative") # these are the nrc options of sentiment labels

## This is a code starter for your take home exercise
## If you want to practice the code on other lexicons
#1 : Take a look at the sentiment for bing
bing_data <- subset(sentiments, lexicon == "bing")
unique(bing_data$sentiment)

#2 Take a look at the sentiment value for afinn
afinn_data <- subset(sentiments, lexicon == "afinn")
summary(afinn_data$value)

###########################
# Words that are doubly labeled
##########################
sentiments %>%
  count(word, lexicon) %>%
  count(word) %>%
  filter(n > 1)

