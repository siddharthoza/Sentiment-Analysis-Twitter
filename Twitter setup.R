Packages: httpuv, base64enc, searchTwitter
library("twitteR")
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem") #read this was necessary for Windows machines
consumer_key <- 'LngC2BuLdecrGKN9q3Eu7DHL5'
consumer_secret <- 'tRTvxd17hbsCXmkXdcFO1NKTluZyOLQYfB0yHhAkpn4JuLSa0V'
access_token <- '123-sid'
access_secret <- 'sid'
setup_twitter_oauth(consumer_key,consumer_secret)
?setup_twitter_oauth()
?oauth_listener