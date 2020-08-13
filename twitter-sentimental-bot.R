requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

consumerKey <- "TYPE CONSUMER KEY HERE"
consumerSecret <- "TYPE consumerSecret HERE"
accessToken <- "TYPE accessToken HERE"
accessTokenSecret <- "TYPE accessTokenSecret HERE"

options(httr_oauth_cache = T)

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

worldCup <- searchTwitter("World Cup 2019" ,n=3000,lang = 'en')

worldCup_text <- sapply(worldCup, function(x) x$getText())

worldCup_text_corpus <- Corpus(VectorSource(worldCup_text))
worldCup_text_corpus <- tm_map(worldCup_text_corpus,removePunctuation)
worldCup_text_corpus <- tm_map(worldCup_text_corpus,content_transformer(tolower))
worldCup_text_corpus <- tm_map(worldCup_text_corpus, function(x)removeWords(x,stopwords()))
worldCup_text_corpus <- tm_map(worldCup_text_corpus,removeWords,c("are","that","is"))

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)

worldCup_text_corpus <- tm_map(worldCup_text_corpus,content_transformer(removeURL))

worldCup_2 <- TermDocumentMatrix(worldCup_text_corpus)
worldCup_2 <- as.matrix(worldCup_2)
worldCup_2 <- sort(rowSums(worldCup_2),decreasing = TRUE)
worldCup_2 <- data.frame(word = names(worldCup_2),freq=worldCup_2)

head(worldCup_2,10)

barplot(worldCup_2[1:10,]$freq,las=2,names.arg = worldCup_2[1:10,]$word,col = "yellow", main = "most frequent word", ylab = "Word frequencies")

set.seed(1234)
wordcloud(worldCup_text_corpus,min.freq = 1,max.words = 80,scale = c(2.2,1),colors = brewer.pal(8,"Dark2"),random.color=T,random.order=F)

pos.words <- read.csv("/home/manas/Downloads/pos.csv")
neg.words <- read.csv("/home/manas/Downloads/neg.csv")

pos.words <- scan("/home/manas/Downloads/pos.csv",what = 'character')
neg.words <- scan("/home/manas/Downloads/neg.csv",what = 'character')

pos.words = c(pos.words, 'new','nice' ,'good', 'horizon','india')
neg.words = c(neg.words, 'wtf', 'behind','feels', 'ugly', 'back','worse' ,'out', 'shitty', 'bad', 'no','freaking','sucks','horrible')

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

test <- ldply(worldCup,function(t) t$toDataFrame() )
result <- score.sentiment(test$text,pos.words,neg.words)

summary(result$score)
hist(result$score,col ="yellow", main ="Score of tweets", ylab = " Count of tweets")

count(result$score)

library(ggplot2)
qplot(result$score,xlab = "Score of tweets")

count(result$score)

qplot(result$score,xlab = "score of tweets")