#########################################
### Example with Netflix data######
#########################################
install.packages("xxxxx")
library(xxx)
tuesdata <- tidytuesdayR::xxx('2021-04-20')
netflix <- xxx$netflix_titles

library(xxx)
library(xxx)

#let's look at the data
View(xxx[xxx,])
unique(netflix$xxx)
unique(netflix$xxxx)
##############################
##############################
#need to rename the description variables as "text"
#this is good coding practice
colnames(xxx)[xxxx] <- "xxx"
#We need one token per row , 
#so that the structure is simialar from our bacon tokenizing script.

library(xxxx)
tidy_netflix <- netflix %>%
                  xxxxx(word, xxxx)
print(tidy_netflix)

 #removing stop words
data(xxxx)
netflix_no_stop <- tidy_netflix %>%
                        xxxx(stop_words)
print(netflix_no_stop)
#printing the count frequencies for each token without stop words
netflix_no_stop %>%
  xxxx(word, sort=TRUE)

#plotting the token frequencies:
library(xxxx)
freq_hist <-xxxx %>%
  count(word, sort=TRUE) %>%
  filter(n>200) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  xxxxx(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(xxxx)
