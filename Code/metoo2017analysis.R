## Content and Sentiment Analysis: 28K #Metoo tweets in 2017-18

#Since I haven't put my computer through enough already, I'm going to re-run
#my content and sentiment analysis with an even bigger dataset that covers the 
#time between my initial subset of Tarana Burke (TB) tweets and the 15k tweets
#from 2019. Due to my sample sizes being vastly different between these 3 time-
#points it would be difficult to draw direct comparisons between the 3, but non-
#theless, I hope I can provide some interesting insights regarding the usage of 
#the metoo hashtag on twitter over time with the available data. 

## Setting wd and calling the data
setwd("/Users/alexandramccoy/Downloads")

metoo <- read.csv("MeToo_tweets-2.csv", header = T)
str(metoo)

##load Vector/text data as a Corpus via tm package
library(tm)

corpus <- iconv(metoo$text, sub='')
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

## Data cleaning for content analysis: 
# Making everything lowercase so it's easier to eval
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

# Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

# Removing numbers
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

# Removing 'stop words' -- important for text analysis
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

# Removing urls
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

##Construct a Document Matrix so we can run content analysis. 

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

## Visualizing the content:

## Bar plot of document Matrix 
w <- rowSums(tdm)
w <- subset(w, w>=1500)
barplot(w,
        las = 2,
        col = rainbow(50))

## Wordcloud on these initial tweets:
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

# Since this dataset is so big, and the search term for these tweets were #metoo,
# I'm going to remove the term metoo and amp (code for a pic/vid embedded in 
# twitter) to look at these visuals again. 
#I'm adding in some other words to remove here before plotting, because when I 
#first ran the barplot after cleaning I was getting some of the stop words/non-
#relevant terms that I thought I already removed. 

cleanset_sub <- tm_map(cleanset, removeWords, c("metoo", "amp", "many", "say",
                                                "can", "just", "dont", "well",
                                                "didnt","ive","cant", "will"))

# This also means that I need to make a new document matrix for the metoo-less
#subset:
tdm_sub <- TermDocumentMatrix(cleanset_sub)
tdm_sub <- as.matrix(tdm_sub)
tdm_sub[1:10, 1:20]

## Re-running content visuals without metoo:
#barplot:
w_sub <- rowSums(tdm_sub)
w_sub <- subset(w_sub, w_sub>=1000)
barplot(w_sub,
        las = 2,
        col = rainbow(50))

# wordcloud
w_sub <- sort(rowSums(tdm_sub), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w_sub),
          freq = w_sub,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(2.5, 0.3),
          rot.per = 0.7)

## Sentiment analysis for 2017-2018 tweets 
#Loading in required packages
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# First, I need to create another df that is not a corpus this time

tweets <- iconv(metoo$text, sub='')
s <- get_nrc_sentiment(tweets)
head(s)

#Visualizing sentiment analysis: 

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets; 2017')

## Top tweets: Most favorited

metoo %>% 
  arrange(-favorites) %>%
  top_n(10, favorites) %>% 
  select(created, text, favorites)


