library(rvest)
library(XML)
library(magrittr)

## Race 3 Movie Review ##

race3 <- NULL
rev <- NULL
url <- "https://www.imdb.com/title/tt7431594/reviews?ref_=tt_ql_"
murl <- read_html(as.character(paste(url,1, sep = "")))
rev <- murl %>% html_nodes(".show-more__control") %>% html_text()
race3 <- c(race3,rev)

write.table(race3, "race.txt")
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