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
#           Before proceeding, user must set up a Twitter API Application at http://apps.twitter.com
#           
# Run lines 2 - 11 first.
require(twitteR)
require(ROAuth)
require(curl)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

# Replace ConsumerKey and ConsumerSecret with your API valudes from apps.twitter.com
consumerKey <- 'XXXXXXXXXX' 
consumerSecret <- 'XXXXXXXXXX' 
Cred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)

# Pause here to auth through browser, enter PIN, press enter. Then, run lines 12 - 16
# Replace access_token and access_secret with your API valudes from apps.twitter.com
access_token = 'XXXXXXXXXX' 
access_secret= 'XXXXXXXXXX'
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

# Pulls tweets for a given search string since a provided date. Uncomment/Comment for variation
tweets = searchTwitter("#MondayMotivation",n=2500, retryOnRateLimit=120, lang="en", since="2017-11-13", resultType="recent")
# Pulls tweets for a given search string for a given geographical circle.
# tweets = searchTwitter("#MondayMotivation",n=2500, retryOnRateLimit=120, lang="en", geocode="37.7749,-122.4194,100 mi", since="2017-10-29", resultType="recent")

tweets = do.call("rbind", lapply(tweets, as.data.frame))
# Set directory to a specific directory on your workstation to save tweets
write.csv(tweets,file="C:/Path/to/your/file/tweets.csv")
some_tweets = tweets 
#some_txt = sapply(some_tweets, function(x) x$getText())
some_txt = some_tweets$text
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
some_txt = gsub("@\\w+", "", some_txt)
some_txt = gsub("&amp;\\w+","", some_txt)
some_txt = gsub("[[:punct:]]", "", some_txt)
some_txt = gsub("[[:digit:]]", "", some_txt)
some_txt = gsub("http\\w+", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
some_txt = gsub("[^[:graph:]]", " ", some_txt)
some_txt = gsub("amp","", some_txt)
some_txt = gsub("http","", some_txt)
some_txt = gsub("htt","", some_txt)
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
some_txt = sapply(some_txt, try.error)

# replace monidaymotivation with same hashtag from search string.
some_txt = gsub("mondaymotivation", "", some_txt)
corpus = Corpus(VectorSource(some_txt))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus,PlainTextDocument)
corpus = iconv(corpus, to = "utf-8", sub="")

# Execute WordCloud
require(wordcloud)
wordcloud(corpus, scale=c(4,0.5), max.words=150, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

