# Wordcloud - Twitter on "Renewable Energy"

# Rebeca Carrillo-Clayton, 2015
# rc@rcclayton.com


library(stringr)
library(tm)
library(wordcloud)

# Installs and loads a package if necessary
EnsurePackage<-function(x)
{
  x <- as.character(x)
  if (!require(x,character.only=TRUE))
  {
    install.packages(pkgs=x)
    require(x, character.only=TRUE)
  }
}
# Load packages for working with twitteR
PrepareTwitter<-function()
{
  EnsurePackage("bitops")
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  EnsurePackage("twitteR")
  EnsurePackage("ROAuth")
}

PrepareTwitter()

credential <- OAuthFactory$new(consumerKey="xNxTdpfkU44g6opYMJnQ",
                               consumerSecret="3jsqIxwfRGIm4LubIB9bLYXFhZAclOy1lQRx2UHCYM",
                               accessURL="https://api.twitter.com/oauth/access_token",
                               authURL="https://api.twitter.com/oauth/authorize",
                               requestURL="https://api.twitter.com/oauth/request_token")

credential$handshake()

# TweetFrame() - Return a dataframe based on a search of Twitter

TweetFrame <- function(searchTerm, maxTweets)
{
  twList <- searchTwitter(searchTerm, n=maxTweets)
  
  return(do.call("rbind", lapply(twList, as.data.frame)))
}

# Search for tweets
data <- TweetFrame("#climate", 500)
tweets  <- data$text

head(tweets)

# refine text from tweets
tweets <- str_replace_all(tweets, pattern = "http://t.co/[a-z,A-Z,0-9]*", replacement = "")
tweets <- str_replace_all(tweets, pattern = "RT @[a-z,A-Z]*",replacement = "")
tweets <- str_replace_all(tweets, pattern = "@[a-z,A-Z]*",replacement = "")
tweets <- str_replace_all(tweets, pattern = "#[a-z,A-Z]*", replacement = "")

tweetCorpus <- Corpus(VectorSource(tweets))
tweetCorpus <- tm_map(tweetCorpus, stripWhitespace)
tweetCorpus <- tm_map(tweetCorpus, tolower)
tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
tweetCorpus <- tm_map(tweetCorpus, removeWords, stopwords("english"))

# create data frame
tweetTDM <-TermDocumentMatrix(tweetCorpus)


sortedMatrix <- sort(rowSums(as.matrix(tweetTDM)), decreasing = T)

# create word cloud
cloud <- wordcloud(names(sortedMatrix), sortedMatrix)


# second word cloud, with data about "#renewableenergy"
rdata <- TweetFrame("#renewableenergy", 500)
tweets  <- rdata$text

head(tweets)

# refine text from tweets
tweets <- str_replace_all(tweets, pattern = "http://t.co/[a-z,A-Z,0-9]*", replacement = "")
tweets <- str_replace_all(tweets, pattern = "RT @[a-z,A-Z]*",replacement = "")
tweets <- str_replace_all(tweets, pattern = "@[a-z,A-Z]*",replacement = "")
tweets <- str_replace_all(tweets, pattern = "#[a-z,A-Z]*", replacement = "")

tweetCorpus <- Corpus(VectorSource(tweets))
tweetCorpus <- tm_map(tweetCorpus, stripWhitespace)
tweetCorpus <- tm_map(tweetCorpus, tolower)
tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
tweetCorpus <- tm_map(tweetCorpus, removeWords, stopwords("english"))

# create data frame
tweetTDM <-TermDocumentMatrix(tweetCorpus)
sortedMatrix <- sort(rowSums(as.matrix(tweetTDM)), decreasing = T)


# second word cloud
cloud2 <- wordcloud(names(sortedMatrix), sortedMatrix)