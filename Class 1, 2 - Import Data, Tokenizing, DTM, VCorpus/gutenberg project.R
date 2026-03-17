############
#Take a look at project Gutneberg
#http://www.gutenberg.org/wiki/Main_Page
###########
#We will use these 4 books by Wells:
# The Time machine ID:35
# The war of the worlds ID:36
#The invisible man ID:5230
# The island of Doctor Moreau ID: 159
############################################
#install.packages("gutenbergr")
data(stop_words)
library(xxxxx)
mydata <- gutenberg_download(c(xxxxxx), mirror = "http://mirrors.xmission.com/gutenberg/")
tidy_mydf <- mydata %>%
                xxxxx(word, text) %>%
                xxxxx(stop_words)
print(xxxxx)
#counting frequencies for tokens
tidy_mydf %>%
  xxxxx(word, sort=TRUE)
  
###########
#We will use another set of 5 books by Bronte Sisters:
# Jane Eyre ID:1260
# Wuthering Heights ID:768
# The Tenant of Wildfell Hall ID:969
# Villette ID:9182
# Agnes Grey ID: 767

data(stop_words)
library(gutenbergr)
bronte_sisters <- gutenberg_download(c(xxxxx,xxxxx,xxxxx,xxxxx,xxxx), mirror = "http://mirrors.xmission.com/gutenberg/")
tidy_bronte <- xxxxxx %>%
  unnest_tokens(word, xxxxx) %>%
  xxxxxx(stop_words)
print(tidy_bronte)
#counting frequencies for tokens
tidy_bronte %>%
  count(xxxxx, sort=TRUE)

#############################################
####We want to combine all the datasets and do frequencies 
#############################################
library(tidyr)
frequency <- bind_rows(mutate(tidy_mydf, author="xxxx"),
                       mutate(tidy_bronte, author= "xxxxxx"),
                       mutate(tidy_janeausten_no_stop, author="xxxxx")
                       )%>%#closing bind_rows
                mutate(word=str_extract(word, "[a-z']+")) %>%
                count(author, word) %>%
                group_by(author) %>%
                mutate(proportion = n/sum(n))%>%
                select(-n) %>%
                spread(author, proportion) %>%
                gather(author, proportion, `Bronte Sista`, `Wells`)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`xxxxxx`, 
                      color = abs(`xxxxxx`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Jane Austen", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

xxxxxx(data=frequency[frequency$author == "xxxxxx",],
         ~proportion + `Jane Austen`)

xxxxxx(data=frequency[frequency$author == "xxxxx",],
         ~proportion + `Jane Austen`)
