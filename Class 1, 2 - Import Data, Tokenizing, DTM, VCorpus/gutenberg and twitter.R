
library("twitteR")
library("tm")

#necessary file for Windows
setwd("xxxxxx")
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'xxxxx'
consumer_secret <- 'xxxxx'
access_token <- 'xxxxxx'
access_secret <- 'xxxxxx'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
USA <- twitteR::searchTwitter('#USA + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)

EU <- twitteR::searchTwitter('#EU + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU)

ASIA <- twitteR::searchTwitter('#Asia + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(ASIA)

print(stop_words)
cust_stop <- data_frame(word=c("http", "https", "rt", "t.io"),
                        lexicon=rep("cust", each=4)
                        )
  
  
tidy_usa <- d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

tidy_eu <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_asia <- a %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#################################################
### Combining all 3 tidy data frames ############
#################################################

library(tidyr)
frequency <- bind_rows(mutate(tidy_usa, author="xxxx"),
                       mutate(tidy_eu, author= "xxxx"),
                       mutate(tidy_asia, author="xxxxx")
                        )%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `xxxx`, `xxxx`)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`xxxx`, 
                      color = abs(`xxxxxx`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "USA", x=NULL)


cor.test(data=frequency[frequency$author == "xxxxx",],
         ~proportion + `USA`)

cor.test(data=frequency[frequency$author == "xxxxx",],
         ~proportion + `USA`)

