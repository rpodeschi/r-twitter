# Program:  twitterSentiment.r
#   
# Date:     November 19, 2017    
# Author:   RJ Podeschi
#
# Required Libraries: twitteR, ROAuth, curl, tm, streamR, base64enc, caTools, SnowballC,
#                     plyr, stringr, ggplot2,
# 
# Purpose:  Connect to Twitter using Developer API, retrieve a pre-defined set of Tweets,
#           pre-process data, and perform sentiment analysis scores.
#
# Notes:    Adopted from: Hill, S. & Scott, R. (2017). An Approach to Harvesting, Cleaning, and
#             Analyzing Data from Twitter using R. Information Systems Education Journal.
#             Retrieved from: http://isedj.org/2017-15/n3/ISEDJv15n3p42.pdf

#           Sentiment Analysis also adopted by: https://analyzecore.com/2014/04/28/twitter-sentiment-analysis/
#           See https://sites.google.com/site/miningtwitter/questions/sentiment/analysis for more examples
#
#           Before proceeding, user must set up a Twitter API Application at http://apps.twitter.com
#           see authenticate.R
# 

# libraries

library(tm)
library(streamR)
library(base64enc)
library(caTools)
library(SnowballC)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)

# Pulls tweets for a given search string since a provided date. Uncomment/Comment for variation
tweets.list <- searchTwitter("#MondayMotivation",n=2500, retryOnRateLimit=120, lang="en", since="2017-11-13", resultType="recent")
# Pulls tweets for a given search string for a given geographical circle.
# tweets = searchTwitter("#MondayMotivation",n=2500, retryOnRateLimit=120, lang="en", geocode="37.7749,-122.4194,100 mi", since="2017-10-29", resultType="recent")

tweets.df = twListToDF(tweets.list)

# Set directory to a specific directory on your workstation to save tweets
write.csv(tweets.df, file='data/tweets.csv', row.names=F)

# Create function to score sentiment
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words){
    sentence = gsub('[[:punct:]]', "", sentence)
    sentence = gsub('[[:cntrl:]]', "", sentence)
    sentence = gsub('\\d+', "", sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# Supply location of your project folder
# Read in location of positive and negative word lists
pos <- scan('positive-words.txt', what='character', comment.char=';')
neg <- scan('negative-words.txt', what='character', comment.char=';')
# Add a few more words to each list -- can customize
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

DataSetTweets <- read.csv("data/tweets.csv")
DataSetTweets$text <- as.factor(DataSetTweets$text)
DataSetTweets$text <- as.factor(DataSetTweets$text)

# Score sentiment for each tweet. 
tweets.scores = score.sentiment(DataSetTweets$text, pos.words, neg.words, .progress='text')

# Write scores to tweetScores.csv
path <- "c:/users/rpodeschi/Documents/r-twitter/data"
write.csv(tweets.scores, file=paste(path,"tweetScores.csv", sep=""), row.names=TRUE)

# View histogram of scores. Scores > 0 are positive, Scores < 0 are negative. 0 is neutral
ggplot(tweets.scores, aes(x=score)) + geom_histogram(bins=27) + theme_bw()

# Create sentiment score categories
some_tweets$score <- tweets.scores$score
write.csv(some_tweets, file="data/tweetsandscores.csv", row.names=TRUE) 
some_tweets$scorescat[some_tweets$score < 0] <- "Negative"
some_tweets$scorescat[some_tweets$scores >= 0] <- "Not Negative"
some_tweets$scorescat = as.factor(some_tweets$scorescat)
corpus = tm_map(corpus, stemDocument, language = "english")
corpus = tm_map(corpus,PlainTextDocument)

#Build treemap -- not sure this is working properly
frequencies = DocumentTermMatrix(corpus)
findFreqTerms(frequencies, lowfreq=20)
sparse = removeSparseTerms(frequencies, 0.995) 
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$scorescat = some_tweets$scorescat
split = sample.split(tweetsSparse$scorescat, SplitRatio = 0.7)
train = subset(tweetsSparse, split==TRUE)
test = subset(tweetsSparse, split==FALSE)

# Run Confusion Matrix and Prediction Model
tweetTREE = rpart(scorescat ~ ., data=train, method="class")
predictTREE = predict(tweetTREE, newdata=test, type="class")




