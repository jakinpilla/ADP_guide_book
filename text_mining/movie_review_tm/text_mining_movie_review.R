setwd('C:/Users/dsc/adp_guidebook/text_mining/movie_review_tm')

library(rJava)

# install.packages("rvest")
library(rvest)

# install.packages("httr")
library(httr)

# install.packages("KoNLP")
library(KoNLP)
useSejongDic()

# install.packages("stringr")
library(stringr)

# install.packages("tm")
library(tm)

# install.packages("qgraph")
library(qgraph)

# install.packages('xml2')
library(xml2)

library(tidyverse)

extractNoun('연습을 해보고자 한다. 명사가 잘 추출되는지 보자. 빨간색으로 글씨를 쓰고 있다.')

# movie "sing" reviews
url_base <- 'http://movie.daum.net/moviedb/grade?movieId=99056&type=netizen&page='

all.reviews <- c()

for(page in 1:50) {
  url <- paste(url_base, page, sep = "")
  htxt <- read_html(url)
  comments <- html_nodes(htxt, 'div') %>% html_nodes('p')
  reviews <- html_text(comments)
  reviews <- repair_encoding(reviews, from = 'utf-8')
  if(length(reviews) == 0) {break}
  reviews <- str_trim(reviews)
  all.reviews <- c(all.reviews, reviews)
}

all.reviews <- all.reviews[!str_detect(all.reviews, "평점" )]

all.reviews[1:10]
class(all.reviews)
df_review = data.frame(review = all.reviews)
write.csv(df_review, './movie_review_tm_data/movie_sing_review.csv', fileEncoding = 'utf-8')

## 명사/형용사 추출 함수 생성

# ko.words <- function(doc) {
#   d <- as.character(doc)
#   pos <- paste(SimplePos09(d))
#   extracted <- str_match(pos, '([가-힣]+)/[NP]')
#   keyword <- extracted[,2]
#   keyword[!is.na(keyword)]
# }

# loading data from local file
library(readr)
all.reviews <- read_csv("./movie_review_tm_data/movie_sing_review.csv")['review']
head(all.reviews)
is.na(all.reviews$review)

# remove NA
df_review <- data.frame(review = all.reviews$review[complete.cases(all.reviews)], stringsAsFactors = F)
head(df_review)

# 명사만 추출하는 함수 생성
ko.words <- function(doc){
  d <- as.character(doc)
  extractNoun(d)
}

## wordcloud
# install.packages("RColorBrewer")
library(RColorBrewer)
# install.packages("wordcloud")
library(wordcloud)

str(df_review)
df_review$review[1:100]
nouns <- sapply(df_review$review, extractNoun, USE.NAMES = F)
nouns[1:10]

final_nouns <- ko.words(nouns)
final_nouns[1:10]
wordcount <- table(unlist(final_nouns))

# wordcount
# sort(wordcount, decreasing = TRUE)
wordcount <- table(unlist(final_nouns))
pal <- brewer.pal(9, "Set1")
wordcloud(names(wordcount), freq = wordcount, scale = c(5, 1), 
          rot.per = 0.25, min.freq = 2, colors = pal, random.color = T, random.order = F)


## Corpus $ TermDocumentMatrix
cpc <- VCorpus(VectorSource(df_review$review))
# cpc
my.tdm <- TermDocumentMatrix(cpc, 
              control=list(tokenize = ko.words,
                      removePunctuation = T,
                      removeNumbers = T,
                      wordLength = c(2, 6),
                      weighting =weightBin))
dim(my.tdm) # 328, 317 
# 317개의 문서에서 328개의 단어들이 추출

inspect(my.tdm[60:70, 1:50])
inspect(my.tdm[100:200, 70:100])

# tdm to matrix
tdm.matrix <- as.matrix(my.tdm)
tdm.matrix
rownames(tdm.matrix)[1:100]

# 단어 사전
my_dict <- c('영화', '감동', '재미', '음악', '공감', '애니메이션', '어른', 
             '추천', '생각', '공감', '스토리', '교훈', '눈물')

my.tdm <- TermDocumentMatrix(cpc, 
                             control=list(tokenize = ko.words,
                                          removePunctuation = T,
                                          removeNumbers = T,
                                          wordLength = c(2, 6),
                                          dictionary=my_dict,
                                          weighting =weightBin))

inspect(my.tdm[, 60:70])
findAssocs(my.tdm, '감동', .1) # numeric(0)
















































