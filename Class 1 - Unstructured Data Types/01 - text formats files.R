#install.packages("textreadr")
library(xxx)
#Importing all .txt files from one directory # a txt works like a csv file with multiple rows
setwd("xxxx")
nm <- list.files(path="xxxx")
#using read document to import the data:
my_data <- xxxx(file=nm[1]) #This comes out as a vector
my_data_together <- paste(xxxx, collapse = " ") # This will give us a concatenated vector

my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))

#Importing all .doc files from one directory
#install.packages("textshape") #for some reason textreadr has issues getting textshape
#install.packages("textreadr")
library(xxxx)
setwd("xxxxx")
nm <- list.files(path="xxxx")
my_doc_text <- do.call(rbind, lapply(nm, function(x) read_doc(file=x)))


# Importing all PDF files from the same folder
#install.packages("pdftools")
library(xxxx) # we need this library to use pdf_text
setwd("xxxx")
nm <- list.files(path="xxxx")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))


#Scraping wesites from text
#install.packages("rvest")
#install.packages("magrittr")
library(magrittr)
library(rvest)
lego_movie <- html("http://www.imdb.com/title/tt1490017/")
lego_movie %>%
  html_node("strong span") %>%
  html_text()


###############################################################
######Querying Twitter for shares, like Trump tweets###########
###############################################################
#install the necessary packages
#install.packages("twitteR")
#install.packages("tm")

library("xxx")
library("xxx")

#necessary file for Windows
setwd("xxxx")
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'xxxx'
consumer_secret <- 'xxxx'
access_token <- 'xxxx'
access_secret <- 'xxxx'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
USA <- twitteR::searchTwitter('#USA + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)
write.csv(d, file='xxxxx')

EU <- twitteR::searchTwitter('#EU + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU) 
write.csv(e, file='xxxxx')

Asia <- twitteR::searchTwitter('#Asia + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(Asia) 

