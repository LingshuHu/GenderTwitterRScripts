## Collect Twitter data

library(rtweet)

## search for tweets
rt <- search_tweets(
  ## use keywords
  "#abortion OR abortion OR prolife OR prochoice  
  until:2017-10-16 since:2017-10-15", 
  n = 18000, ## number of tweets to collect
  include_rts = TRUE, ## whether to include retweet type of tweets
  lang = "en", ## language of tweets
  retryonratelimit = TRUE
)