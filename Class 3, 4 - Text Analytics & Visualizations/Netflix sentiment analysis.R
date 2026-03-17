
###############################################################
### Sentiment analysis with Netflix######
###############################################################

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

netflix_token <- netflix %>%
  unnest_tokens(word, text)

nrcsurprise <- get_sentiments("xxxxx") %>%
                  filter(xxxxxxx == "xxxx")

#inner joining the India movies and the surprise sentiments
netflix_token %>%
  filter(xxxxxx == "xxxx") %>%
  inner_join(xxxxx) %>%
  xxxxx(word, sort=T)
  
########################################################
##### Comparing different sentiment libraries on Netflix ####
########################################################

india <- netflix_token %>%
          filter(country == "India")

afinn <- india %>%
  inner_join(get_sentiments("afinn"))%>%
 summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  india%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  india %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts <- xxxxx %>%
  inner_join(get_sentiments("xxxxx")) %>%
  count(xxxxxxx, xxxxxx, sort=T) %>%
  ungroup()

bing_counts

xxxxxx %>%
  group_by(xxxxxx) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()