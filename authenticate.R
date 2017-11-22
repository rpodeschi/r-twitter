#   Program:     authenticate.R
#   Date:        November 21, 2017
#   Author:      Jake Tolbert & RJ Podeschi
# 
#   Purpose:     Authenticate using Twitter API from https://apps.twitter.com
    
# libraries ---------------------------------------------------------------

library(twitteR)
library(ROAuth)
library(curl)

# variables ---------------------------------------------------------------

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'


source('config.R')


Cred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)

# authenticate ------------------------------------------------------------

save(Cred, file='twitterauthentication.Rdata')
load('twitterauthentication.Rdata') 
setup_twitter_oauth(consumerKey,consumerSecret,access_token,access_secret)
# Twitter API now authenticated