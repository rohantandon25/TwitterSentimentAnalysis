library(twitteR)
library(ROAuth)
require(RCurl)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )

api_key <- "ZkbKOcrwHZRB6kWYbnTHmCwjo"

api_secret <- "t4LtacGkPU3vmEMUoV47XrbSo3BU6kBoIFpp3b5RsUX6N0twN1"

access_token <- "767816885765300224-svkvzrZ4Kb5nrSz0xfKnCezrKYjQdKK"

access_token_secret <- "NQirR9pn9KtTWzGwONT7KlwBVvm7ZZEGxgbItQcTHFBhr"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets = searchTwitter("#POTUS", n=500) ###search for tweets with the hashtag POTUS

#filtering tweets regex method gsub():
tweets = laply(tweets,function(t)t$getText())
#to remove emojis
tweets <- iconv(tweets, 'UTF-8', 'ASCII')
#removing punctuation
tweets = gsub('[[:punct:]]', '', tweets)
tweets = gsub('[[:cntrl:]]', '', tweets)
tweets = gsub('\\d+', '', tweets)
#convert to lower case:
tweets = tolower(tweets)

#creating a corppus
corpus = Corpus(VectorSource(tweets))

#removing stopwords
corpus = tm_map(corpus, removeWords, stopwords(kind="en"))

df<-data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                      stringsAsFactors=F)

#df <- do.call("rbind", lapply(m, as.data.frame))

write.csv(df,file="/Users/rohantandon/Desktop/CS_590AC/sentiment/tweets_potus2.csv")


