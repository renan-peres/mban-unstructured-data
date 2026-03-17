#####################################################
################## Corpus object ####################
#####################################################
library(tm)
data("xxxx")#50 articles from Reuters
xxxx # we get a VCorpus

#We want to convert this to a tidy format that has 
#one row per document

acq_tidy <- tidy(xxxx)
summary(acq_tidy)

acq_tokens <- acq_tidy %>%
                select(-places) %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words, by = 'word')
#most common words
acq_tokens %>%
  count(word, sort=TRUE)

#####################################################
######### Corpus object with PDF files###############
#####################################################

#Import the PDF files that you downloaded from mycourses
library(pdftools) # we need this library to use pdf_text
library(tm)
setwd("xxxxx")
nm <- list.files(path="xxxxx")

# the readPDF function doesn't actually read the PDF files, 
#the read PDF creates a function to read in all the PDF files
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(nm), 
                   readerControl = list(reader = Rpdf))

opinions # I want to see the VCoprus content
opinions[[1]] # I want the first doc, this is a list so [[]] needs to be applied
#let's take a look at the Corpus that was created 
#if you want to get some metadata from the i-th object:
opinions[[1]]$xxxxx$xxxx

#Exercises:
#Try to find the author's name for the 7th document
#try to find the document ID in for the 4th document

