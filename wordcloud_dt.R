library(twitteR)
library(stringr)
library(wordcloud)
library(tm)

#connect to API
#download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='/Users/rohantandon/Desktop/CS_590AC/sentiment/cacert.pem')

api_key <- "ZkbKOcrwHZRB6kWYbnTHmCwjo"

api_secret <- "t4LtacGkPU3vmEMUoV47XrbSo3BU6kBoIFpp3b5RsUX6N0twN1"

access_token <- "767816885765300224-svkvzrZ4Kb5nrSz0xfKnCezrKYjQdKK"

access_token_secret <- "NQirR9pn9KtTWzGwONT7KlwBVvm7ZZEGxgbItQcTHFBhr"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

mach_tweets = searchTwitter("#DonaldTrump", n=500, lang="en")

mach_tweets = laply(mach_tweets,function(t)t$getText())
mach_tweets <- str_replace_all(mach_tweets, "#\\w+", "")
mach_tweets <- str_replace_all(mach_tweets, "@\\w+", "")
mach_tweets = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", mach_tweets)
mach_tweets <- gsub("http\\w+", "", mach_tweets)
mach_tweets = gsub('[[:punct:]]', '', mach_tweets)
mach_tweets = gsub('[[:cntrl:]]', '', mach_tweets)
mach_tweets = gsub('\\d+', '', mach_tweets)
#to remove emojis
mach_tweets <- iconv(mach_tweets, 'UTF-8', 'ASCII')
# and convert to lower case:
mach_tweets = tolower(mach_tweets)

# create a corpus
mach_corpus = Corpus(VectorSource(mach_tweets))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("machine", "learning", stopwords("english")), 
                                        removeNumbers = TRUE, tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

