######################################################
####### TF-IDF framework in Netflix #######
######################################################


library(dplyr)
library(stringr)
library(tidytext)
#let's look at the data
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"
#we're grouping by the country this time
netflix_token <- netflix %>%
  xxxxxx(word, text) %>%
  xxxxxx(xxxxx, word, sort=TRUE) %>%
  ungroup()

total_words <- netflix_token %>%
                  xxxxxx(xxxxx) %>%
                  xxxxx(total=sum(n))

netflix_words <- left_join(netflix_token, total_words)%>%
                  filter(xxxxx %in% c("xxxxx", "xxxxx", "xxxxx"))

print(xxxxxx)

library(xxxxx)
ggplot(xxxxx, aes(n/total, fill = xxxx))+
  xxxxxxx(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")
#what do the tails represent? 
#answer: exremely common words! 
# we are really interested in the not so common words. 

######################################
########## ZIPF's law ################
######################################

freq_by_rank <- netflix_words %>%
                  group_by(xxxx) %>%
                  mutate(xxxxx = row_number(),
                         `term frequency` = n/total)
freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=country))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################

country_words <- netflix_words %>%
  xxxxxxxx(xxxxx, xxxxxx, n)

country_words # we get all the zeors because we are looking at stop words ... too common

country_words %>%
  xxxxxxx(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
xxxxxxxx %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()
