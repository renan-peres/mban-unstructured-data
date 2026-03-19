## making sure we have the data
library(tidytuesdayR)
library(dplyr)
library(tidytext)
library(stringr)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"
#############################################
### creating a tidy format for United States movies
usa <- netflix %>%
        filter(country== "United States")

tidy_usa <- usa %>%
 unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_usa)

### creating a tidy format for Brazil movies
brazil <- netflix %>%
  filter(country== "Brazil")

tidy_brazil <- brazil %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_brazil)

### creating a tidy format for India movies
india <- netflix %>%
  filter(country== "India")

tidy_india <- india %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_india)

#############################################
####We want to combine all the datasets and do frequencies 
#############################################
library(tidyr)
frequency <- bind_rows(mutate(tidy_usa, author="United States"),
                       mutate(tidy_brazil, author= "Brazil"),
                       mutate(tidy_india, author="India")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brazil`, `India`)

#let's plot the correlograms:
library(scales)
library(ggplot2)
ggplot(frequency, aes(x=proportion, y=`United States`, 
                      color = abs(`United States`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "United States", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$author == "xxxxxx",],
         ~proportion + `United States`)

cor.test(data=frequency[frequency$author == "xxxxxx",],
         ~proportion + `United States`)
