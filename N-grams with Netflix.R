#############################################
###### N-grams and tokenizing ###############
#############################################

library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

netflix_bigrams <- xxxxx %>%
                    xxxxxxx(xxxxxx, xxxxx, token = "xxxxxx", n=2)

netflix_bigrams #We want to see the bigrams (words that appear together, "pairs")

xxxxxxx %>%
  xxxxxx(xxxxx, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- xxxxxxx %>%
                        xxxxxxxx(xxxxxx, c("word1", "xxxxx"), sep = " ")

bigrams_filtered <- xxxxxxx %>%
  filter(!xxxxxx %in% stop_words$word) %>%
  filter(!word2 %in% xxxxxxx)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
                  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts
 
###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################
quadrogram <- netflix %>%
  xxxxxx(xxxxxx, text, token = "xxxxx", n=4) %>%
  separate(xxxxxxxxx, c("xxxxxx", "word2", "xxxxxx", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

bigram_united <- xxxxxx %>%
  xxxxxx(xxxx, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- xxxxxx %>%
  count(country, bigram) %>%
  xxxxxxx(bigram, xxxxxx, n) %>%
  xxxxxx(desc(tf_idf))

bigram_tf_idf

##### lets do the same for a quadrogram

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(country, quadrogram) %>%
  bind_tf_idf(quadrogram, country, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf

######################################################
######## visualising negated words ###################
###### negated words in sentiment analysis ###########
######################################################

negation_tokens <- c("no", "never", "without", "not")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(afinn_data, by=c(word2="word")) %>%
  count(word1, word2, score, sort=TRUE) %>%
  ungroup()
  
negated_words

##############################################
#### we can visuals the negated words ####
negated_words_plot <- function(x){
  negated_words %>%
    filter(word1 == x) %>%
    mutate(contribution = n* score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*score, fill = n*score >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
      coord_flip()
}#closing the negated_words_plot function

negated_words_plot(x="not")
negated_words_plot(x="no")
negated_words_plot(x="without")

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
library(igraph)
bigram_graph <- xxxxx %>%
                  xxxxxxx(n>10) %>%
                  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
