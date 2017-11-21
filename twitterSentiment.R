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
#
#           Before proceeding, user must set up a Twitter API Application at http://apps.twitter.com
#           
# Run lines 19 - 34 first.
require(twitteR)
require(ROAuth)
require(curl)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

# Replace ConsumerKey and ConsumerSecret with your API valudes from apps.twitter.com
consumerKey <- 'XXXXXX' 
consumerSecret <- 'XXXXXX' 
Cred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)

# Pause here to auth through browser, enter PIN, press enter. Then, run lines 12 - 16
# Replace access_token and access_secret with your API valudes from apps.twitter.com
access_token = 'XXXXXX' 
access_secret= 'XXXXXX'
save(Cred, file='twitter authentication.Rdata')
load('twitter authentication.Rdata') 
setup_twitter_oauth(consumerKey,consumerSecret,access_token,access_secret)
# Twitter API now authenticated

# Begin harvesting tweets
require(tm)
require(streamR)
require(base64enc)
require(caTools)
require(SnowballC)
require(ggplot2)

# Pulls tweets for a given search string since a provided date. Uncomment/Comment for variation
tweets.list <- searchTwitter("#MondayMotivation",n=2500, retryOnRateLimit=120, lang="en", since="2017-11-13", resultType="recent")
# Pulls tweets for a given search string for a given geographical circle.
# tweets = searchTwitter("#MondayMotivation",n=2500, retryOnRateLimit=120, lang="en", geocode="37.7749,-122.4194,100 mi", since="2017-10-29", resultType="recent")

tweets.df = twListToDF(tweets.list)

# Set directory to a specific directory on your workstation to save tweets
write.csv(tweets.df, file='C:/Path/to/your/directory/tweets.csv', row.names=F)

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
pos <- scan('C:/Path/to/your/directory/positive-words.txt', what='character', comment.char=';')
neg <- scan('C:/Path/to/your/directory/negative-words.txt', what='character', comment.char=';')
# Add a few more words to each list -- can customize
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

DataSetTweets <- read.csv("C:/Path/to/your/directory/tweets.csv")
DataSetTweets$text <- as.factor(DataSetTweets$text)
DataSetTweets$text <- as.factor(DataSetTweets$text)

# Score sentiment for each tweet. 
tweets.scores = score.sentiment(DataSetTweets$text, pos.words, neg.words, .progress='text')

# Write scores to tweetScores.csv
path<-"C:/Path/to/your/directory/"
write.csv(tweets.scores, file=paste(path,"tweetScores.csv", sep=""), row.names=TRUE)

# View histogram of scores. Scores > 0 are positive, Scores < 0 are negative. 0 is neutral
ggplot(tweets.scores, aes(x=score)) + geom_histogram(bins=27) + theme_bw()
