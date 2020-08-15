install.packages("twitteR")
library("twitteR")
install.packages("ROAuth")
library("ROAuth")
cred <- OAuthFactory$new(consumerKey='43hHJkZa4snEM8vbikYur2C0V', # Consumer Key (API Key)
                         consumerSecret='T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("43hHJkZa4snEM8vbikYur2C0V", # Consumer Key (API Key)
                    "T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM", #Consumer Secret (API Secret)
                    "529590041-AJNiB9OMFcaZuCqzoN99fH4pyfUN8uJ9EXDBwY5B",  # Access Token
                    "frz56qjS1TXT1jmBcIWEmXGFLXQu1X3gUswyvMEdCMOfj")

#registerTwitterOAuth(cred)

Tweets <- userTimeline('imVkohli', n = 3000,includeRts = T)

TweetsDF <- twListToDF(Tweets)
View(TweetsDF)
write.csv(TweetsDF, "Tweets.csv")
getwd()

# Sentimental analysis

install.packages("syuzhet")
library(syuzhet)
library(scales)
library(dplyr)
library(reshape2)
library(tm)
library(topicmodels)
library(slam)

txt <-  readLines(file.choose())
x <- txt
View(x)
str(x)
length(x)


#using tm package#
mydata.corpus <- Corpus(VectorSource(x))
mydata.corpus <- tm_map(mydata.corpus,removePunctuation)
my_stopwords <- readLines(file.choose())
mydata.corpus <- tm_map(mydata.corpus,removeWords,my_stopwords)
mydata.corpus <- tm_map(mydata.corpus,removeNumbers)
mydata.corpus <- tm_map(mydata.corpus,stripWhitespace)

#build a term document matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
as.matrix(mydata.dtm3) 

dtm <- t(mydata.dtm3)
dtm$ncol
dtm$nrow
rowTotals <- apply(dtm,1,sum)
?apply
dtm.new <- dtm[rowTotals>0,]

lda <- LDA(dtm.new,10)

lterm <- terms(lda,10)
lterm

tops <- terms(lda)
tb <- table(names(tops),unlist(tops))
tb <- as.data.frame.matrix(tb)
?unlist

x = readLines(file.choose())

s <- get_nrc_sentiment(x)
head(s)

x[4]
get_nrc_sentiment('apple')

# Bar plot for emotion mining

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')