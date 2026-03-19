################################################
###Pairwise correlations between words #########
################################################

#install.packages("widyr")
library(widyr)
library(tidyr)
library(dplyr)
library(ggraph)
library(igraph)
library(tidytext)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

my_tidy_df <- netflix %>%
  filter(country == "United States") %>%
 unnest_tokens(word, text) %>%
  filter(!word %in% stop_words)

my_tidy_df
#taking out the least common words
word_cors <- xxxx %>%
  group_by(xxxx) %>%
  filter(n() >= 5) %>%
  xxxxxx(word, title, sort=TRUE)
#pairwise_cor() check correlation based on how ofter words appear in the same section

word_cors %>%
  filter(item1 == "xxxxx")
########################################################
####### creating barcharts for correlatoins ############
########################################################

word_cors %>%
  filter(item1 %in% c("xxxxxx", "xxxxx", "xxxxxx", "xxxxx")) %>%
  group_by(xxxxx) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, xxxxx)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

########################################################
####### creating a correlation network #################
########################################################

#this will take some time to run, we will need to wait for the result
# feel free to adjust the geom_node_point to somehting smaller

word_cors %>%
  filter(correlation >.3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()
