library(textreadr)
MBA <- read_document(file="xxxxxx")
MIB <- read_document(file="xxxxxx")
class_combo <- c(MBA, MIB)

my_df <- as.data.frame(matrix(nrow=xxxxx, ncol=xxxxx))

for(z in 1:xxxxx){
  for(i in 1:xxxx){
    my_df[i,z]<- class_combo[i*xxxx+z-xxxxx]
  }#closing z loop
}#closing i loop

my_txt <- my_df$V6
my_txt <- substr(my_txt, start=11 , stop = 10000)

library(dplyr)
mydf <- data_frame(line=1:xxxxxx, text=my_txt)
print(mydf)


library(dplyr)
library(stringr)
library(tidytext)

data(stop_words)
frequencies_tokens_nostop <- mydf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)


print(frequencies_tokens_nostop)

