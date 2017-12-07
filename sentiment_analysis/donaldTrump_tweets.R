library(twitteR)
library(ROAuth)
require(RCurl)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )
library(plyr )

api_key <- "ZkbKOcrwHZRB6kWYbnTHmCwjo"

api_secret <- "t4LtacGkPU3vmEMUoV47XrbSo3BU6kBoIFpp3b5RsUX6N0twN1"

access_token <- "767816885765300224-svkvzrZ4Kb5nrSz0xfKnCezrKYjQdKK"

access_token_secret <- "NQirR9pn9KtTWzGwONT7KlwBVvm7ZZEGxgbItQcTHFBhr"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets_sentiment = searchTwitter("#DonaldTrump", n=5000, lang = "en", since = '2016-08-18', until = '2016-08-25')

tweets_sentiment = laply(tweets_sentiment,function(t)t$getText())
tweets_sentiment <- str_replace_all(tweets_sentiment, "#\\w+", "")
tweets_sentiment <- str_replace_all(tweets_sentiment, "@\\w+", "")
tweets_sentiment = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", tweets_sentiment)
tweets_sentiment <- gsub("http\\w+", "", tweets_sentiment)
tweets_sentiment = gsub('[[:punct:]]', '', tweets_sentiment)
tweets_sentiment = gsub('[[:cntrl:]]', '', tweets_sentiment)
tweets_sentiment = gsub('\\d+', '', tweets_sentiment)
#to remove emojis
tweets_sentiment <- iconv(tweets_sentiment, 'UTF-8', 'ASCII')
# and convert to lower case:
tweets_sentiment = tolower(tweets_sentiment)

#make data frame
df <- do.call("rbind", lapply(tweets_sentiment, as.data.frame))

#write to csv file 
write.csv(df,file="/Users/rohantandon/Desktop/CS_590AC/sentiment/tweets_trump.txt")

data_tweets<-read.csv("/Users/rohantandon/Desktop/CS_590AC/sentiment/tweets_trump.txt", stringsAsFactors = FALSE)

mySentiment <- get_nrc_sentiment(data_tweets$X..i..)

data_tweets<-cbind(data_tweets, mySentiment)

sentimentTotals <- data.frame(colSums(data_tweets[,c(3:12)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets-Trump")



