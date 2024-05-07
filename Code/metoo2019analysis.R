## Content and Sentiment Analysis: 15K #Metoo tweets in 2019

# After testing my data on a smaller scale, I want to scale up with this pre-
# existing #metoo tweet dataset taking roughly 15k tweets in 2019. I'm not sure
# if the size of the sample is going to slow me down, and if it turns too problem-
# atic I may create a random subset of tweets from this dataset to then run the 
# code on 

## Setting wd and calling the data
setwd("/Users/alexandramccoy/Downloads")

metoo2 <- read.csv("MeToo_tweets.csv", header = T)
str(metoo2)

##the next step is to load that Vector or text data as a Corpus via tm package
library(tm)

corpus2 <- iconv(metoo2$Text, sub='')
corpus2 <- Corpus(VectorSource(corpus2))
inspect(corpus2[1:5])

## Next, I need to clean my data so it's suitable for a content analysis. 

# Making everything lowercase so it's easier to eval
corpus2 <- tm_map(corpus2, tolower)
inspect(corpus2[1:5])

# Removing punctuation
corpus2 <- tm_map(corpus2, removePunctuation)
inspect(corpus2[1:5])

# Removing numbers
corpus2 <- tm_map(corpus2, removeNumbers)
inspect(corpus2[1:5])

# Removing 'stop words' -- important for text analysis
cleanset2 <- tm_map(corpus2, removeWords, stopwords('english'))
inspect(cleanset2[1:5])

# Removing urls
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset2 <- tm_map(cleanset2, content_transformer(removeURL))
inspect(cleanset2[1:5])

##Construct a Document Matrix – a table containing the frequency of words.##

tdm2 <- TermDocumentMatrix(cleanset2)
tdm2 <- as.matrix(tdm2)
tdm2[1:10, 1:20]

## Visualizing the content:

## Bar plot of document Matrix 
w2 <- rowSums(tdm2)
w2 <- subset(w2, w2>=500)
barplot(w2,
        las = 2,
        col = rainbow(50))

## Wordcloud on these initial tweets:
library(wordcloud)
w2 <- sort(rowSums(tdm2), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w2),
          freq = w2,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

## Before running a sentiment analysis, I want to rerun these visuals but take out 
# the term metoo since it's just pulling a lot of the weight here (which makes sense
#since the hashtag was the search term for data collection)

 cleanset_sub2 <- tm_map(cleanset2, removeWords, c("metoo", "amp", "also", 
                                                   "get", "like", "isnxxt", "itxxs",
                                                   "nmetoo"))
 
# This also means that I need to make a new document matrix for the metoo-less
#subset:
 tdm_sub2 <- TermDocumentMatrix(cleanset_sub2)
 tdm_sub2 <- as.matrix(tdm_sub2)
 tdm_sub2[1:10, 1:20]
 
## Re-running content visuals without metoo:
 w_sub2 <- rowSums(tdm_sub2)
 w_sub2 <- subset(w_sub2, w2>=1000)
 barplot(w_sub2,
         las = 2,
         col = rainbow(50))
 
 w_sub2 <- sort(rowSums(tdm_sub2), decreasing = TRUE)
 set.seed(222)
 wordcloud(words = names(w_sub2),
           freq = w_sub2,
           max.words = 150,
           random.order = F,
           min.freq = 5,
           colors = brewer.pal(8, 'Dark2'),
           scale = c(2.5, 0.3),
           rot.per = 0.7)
 
# I find that the subset removing the term metoo might actually be more valuable.
# However, for the sentiment analysis I will be using the bigger dataset as I'm 
# not focused on specific terms for that task. 
# note: amp tells us that there's an embedded pic/video in the og tweet?
 
 
## Sentiment analysis on 2019 #metoo tweets: 
# packages:
 library(syuzhet)
 library(lubridate)
 library(ggplot2)
 library(scales)
 library(reshape2)
 library(dplyr)
 
# First, I need to create another df that is not a corpus this time
 
tweets2 <- iconv(metoo2$Text, sub='')
s2 <- get_nrc_sentiment(tweets2)
head(s2)
 
 #Visualizing sentiment analysis: 
 
barplot(colSums(s2),
         las = 2,
         col = rainbow(10),
         ylab = 'Count',
         main = 'Sentiment Scores Tweets; 2019')

## Top tweets: Most favorited

metoo2 %>% 
  arrange(-Favorite_count) %>%
  top_n(10, Favorite_count) %>% 
  select(Text, Favorite_count)
 