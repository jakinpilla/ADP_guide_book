setwd('C:/Users/dsc/adp_guidebook/text_mining')

library(readr)
movie_sing_review <- read_csv("movie_review_tm/movie_review_tm_data/movie_sing_review.csv")

head(movie_sing_review)
movie_sing_review$review[1:10]
sing_review <- paste(movie_sing_review$review, collapse= ' ')
class(sing_review)

sing_review <- gsub("NA", "", sing_review)
sing_review <- gsub('[[:punct:]]', '', sing_review)
sing_review <- gsub('[[:cntrl:]]', '', sing_review)

sing_review

library(KoNLP)
library(wordcloud)
tran <- Map(extractNoun, sing_review)
tran <- unique(tran)
tran

tran <- sapply(tran, function(x){
  Filter(function(y) {
    nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)
  }, x)
})

head(tran, 2)

tran
table(tran)
tran.table <- as.data.frame(table(tran))
head(tran.table)

wordcloud(words=tran.table$tran, freq=tran.table$Freq, min.freq = 3, random.order=F, colors = brewer.pal(5, 'Dark2'))








