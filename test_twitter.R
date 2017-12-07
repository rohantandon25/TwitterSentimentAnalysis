library(twitteR)
library(plyr)
library(stringr)
library(wordcloud)
library(tm)

api_key <- "ZkbKOcrwHZRB6kWYbnTHmCwjo"

api_secret <- "t4LtacGkPU3vmEMUoV47XrbSo3BU6kBoIFpp3b5RsUX6N0twN1"

access_token <- "767816885765300224-svkvzrZ4Kb5nrSz0xfKnCezrKYjQdKK"

access_token_secret <- "NQirR9pn9KtTWzGwONT7KlwBVvm7ZZEGxgbItQcTHFBhr"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets = searchTwitter("#POTUS", n=500)

length(tweets)

Tweets.text = laply(tweets,function(t)t$getText())

pos = scan('/Users/rohantandon/Desktop/CS_590AC/sentiment/positive-words.txt', what='character', comment.char=';')

neg = scan('/Users/rohantandon/Desktop/CS_590AC/sentiment/negative-words.txt', what='character', comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    #to remove emojis
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

analysis = score.sentiment(Tweets.text, pos, neg)
table(analysis$score)
mean(analysis$score)
hist(analysis$score)

