#############################################
###### N-grams and tokenizing ###############
#############################################

library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)

austen_bigrams <- austen_books() %>%
                    unnest_tokens(bigram, text, token = "ngrams", n=2)

austen_bigrams #We want to see the bigrams (words that appear together, "pairs")

austen_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- austen_bigrams %>%
                        filter(bigram !="NA") %>% 
                        separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
                  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts
 
###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################
quadrogram <- austen_books() %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  filter(quadrogram != "NA") %>% 
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram


######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

# install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
                  filter(n>20) %>%
                  graph_from_data_frame()

bigram_graph

# install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
