#####################################################
#### DTM object using Associated Press articles######
#####################################################

library(tm)
library(dplyr)
library(tidytext)
install.packages("xxxxxx")
data("AssociatedPress", package = "xxxxx")
AssociatedPress
#99% of the document-word pairs are zero
terms <- Terms(xxxxx)
terms

ap_td <- tidy(xxxxx)
ap_td

######################################################
#####Converting back from Tidy to DTM ###############
######################################################

ap_td %>%
  cast_dtm(xxxxx, xxxxx, xxxxx )

######################################################
#####Putting the data in a sparse matrix ###############
######################################################

library(Matrix)
n <- ap_td %>%
      cast_sparse(document, term, count)
class(n)
dim(n)

######################################################
#####Converting Jane Austen to DTM ###############
######################################################
library(tidytuesdayR)
library(dplyr)
library(tidytext)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

netflix_dtm <- netflix %>%
                xxxxx(word, text) %>%
                count(title, word) %>%
                xxxxxxx(xxxxx, word, n)

netflix_dtm
