#loading associated Press

library(tm)
library(dplyr)
library(tidytext)
library(tidyr)
data("AssociatedPress", package = "topicmodels")
AssociatedPress

ap_td <- tidy(AssociatedPress)
#View(ap_td) this is to see the table

ap_td %>%
  # cast_dtm(document, term, count) #term is word and count is n
  cast_sparse(document, term, count) #This shows how sparse, this not to be used because its not memory efficient

## LEARNING OUTCOME: 
### This is no good for business analytics but GREAT for ML

ap_td #This format is great for business analytics

df <- ap_td %>%
  mutate(word=term) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T)

data("acq")
acq[[1]]$content
acq[[1]]$meta
acq[[1]]$meta$heading


acq_tidy <- tidy(acq)
acq_tidy %>% 
  unnet_tokens(word, text)
