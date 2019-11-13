# Program:  sendTweet.R
#
# Author:   RJ Podeschi
#
# Date:     November 13, 2019
#
# Purpose:  Demonstrate how to send a tweet using the twitteR package.
#
# Before proceeding, user must set up a Twitter API Application at http://developer.twitter.com
# See authenticate.R

# -------- Import Libraries ------------
library(twitteR)
library(jpeg)

# Send Tweet
tweet <- updateStatus("Sending a tweet using twitteR library. Yay! #IS470")

# Delete Tweet just sent
deleteStatus(tweet)

# Get really nerdy and post a plot you just made of a random normal distribution
t1 <- Sys.time()
gauss <- rnorm(n = 10000)
jpeg('temp.jpg')
plot(density(gauss))
dev.off()
t2 <- difftime(Sys.time(), t1, units = 'secs')
tw <- updateStatus(paste('Finished in', round(t2, 2), 'seconds'), mediaPath = 'temp.jpg')