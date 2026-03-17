###############################################################
######Querying Twitter for shares, like Trump tweets###########
###############################################################
#install the necessary packages
#install.packages("twitteR")
#install.packages("tm")

library("twitteR")
library("tm")

#necessary file for Windows
setwd("/Users/thomaskurnicki/Desktop/Text analytics class/Day 1/Twitter API :: to be posted")
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'XXXXXXXXXXXXXX'
consumer_secret <- 'XXXXXXXXXXXXXXXX'
access_token <- 'XXXXXXXXXXXXXXXXXXX'
access_secret <- 'XXXXXXXXXXXXXXXXXXX'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
USA <- twitteR::searchTwitter('#USA + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)

EU <- twitteR::searchTwitter('#EU + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU)
