# Program:  twitterWordCloud.r
#   
# Date:     November 19, 2017    
# Author:   RJ Podeschi
#
# Required Libraries: twitteR, ROAuth, curl, tm, streamR, base64enc, caTools, SnowballC,
#                     wordcloud
# 
# Purpose:  Connect to Twitter using Developer API, retrieve a pre-defined set of Tweets,
#           pre-process data, and return a WordCloud.
#
# Notes:    Adopted from: Hill, S. & Scott, R. (2017). An Approach to Harvesting, Cleaning, and
#             Analyzing Data from Twitter using R. Information Systems Education Journal.
#             Retrieved from: http://isedj.org/2017-15/n3/ISEDJv15n3p42.pdf
#
#           Before proceeding, user must set up a Twitter API Application at http://developer.twitter.com
#           see authenticate.R
#           

# libraries

library(tidyverse)
library(magrittr)

library(tm)
library(streamR)
library(base64enc)
library(caTools)
library(SnowballC)
library(twitteR)

library(wordcloud)

# Harvest tweets

# Pulls tweets for a given search string since a provided date. Uncomment/Comment for variation
tweets_orig = searchTwitter("#MondayMotivation",n=2500, retryOnRateLimit=120, lang="en", since="2017-11-13", resultType="recent")
# Pulls tweets for a given search string for a given geographical circle.
# tweets = searchTwitter("#MondayMotivation",n=2500, retryOnRateLimit=120, lang="en", geocode="37.7749,-122.4194,100 mi", since="2017-10-29", resultType="recent")

tweets <- do.call("rbind", lapply(tweets_orig, as.data.frame)) %>%
  as.tibble()
  
write.csv(tweets,file="data/tweets.csv")

# process tweets

try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

some_tweets <- tweets %>%
  select(doc_id = id, text, everything()) %>%
  mutate(
    text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
    , text = gsub("@\\w+", "", text)
    , text = gsub("&amp;\\w+","", text)
    , text = gsub("[[:punct:]]", "", text)
    , text = gsub("[[:digit:]]", "", text)
    , text = gsub("http\\w+", "", text)
    , text = gsub("^\\s+|\\s+$", "", text)
    , text = gsub("[^[:graph:]]", " ", text)
    , text = gsub("amp","", text)
    , text = gsub("http","", text)
    , text = gsub("htt","", text)
  )
  
some_tweets$text <-  sapply(some_tweets$text, try.error)

# replace monidaymotivation with same hashtag from search string.
some_tweets %<>%
  mutate(
    text = gsub("mondaymotivation", "", text)
  )

# build corpus from text --------------------------------------------------

corpus  <- some_tweets %>% 
  DataframeSource() %>% 
  Corpus()

corpus  <-  tm_map(corpus, removeWords, stopwords("english"))
corpus  <-  tm_map(corpus, removePunctuation)
corpus  <-  tm_map(corpus, stripWhitespace)
corpus  <-  tm_map(corpus,PlainTextDocument)

# Make WordCloud

corpus_txt = iconv(corpus$content$content, to = "utf-8", sub="")
wordcloud(corpus, scale=c(4,0.5), max.words=150, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


