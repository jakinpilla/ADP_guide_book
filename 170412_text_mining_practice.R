
library(rJava)

install.packages("rvest")
library(rvest)

install.packages("httr")
library(httr)

install.packages("KoNLP")
library(KoNLP)
useSejongDic()

install.packages("stringr")
library(stringr)

install.packages("tm")
library(tm)

install.packages("qgraph")
library(qgraph)

library('xml2')


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

all.reviews

## 명사/형용사 추출 함수 생성

ko.words <- function(doc) {
  d <- as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}

## wordcloud

install.packages("wordcloud")
library(wordcloud)

install.packages("RColorBrewer")
library(RColorBrewer)

str(all.reviews)

nouns <- sapply(all.reviews, extractNoun, USE.NAMES = F)

final_nouns <- ko.words(nouns)

wordcount <- table(unlist(final_nouns))

wordcount

sort(wordcount, decreasing = TRUE)

wordcount <- table(unlist(final_nouns))

pal <- brewer.pal(9, "Set1")

wordcloud(names(wordcount), freq = wordcount, scale = c(5, 1), 
          rot.per = 0.25, min.freq = 2, colors = pal, random.color = T, random.order = F)


## Corpus $ TermDocumentMatrix

cpc <- Corpus(VectorSource(all.reviews))
cpc

tdm <- TermDocumentMatrix(cpc, 
              control=list(tokenize = ko.words,
                      removePunctuation = T,
                      removeNumbers = T,
                      wordLength = c(2, 6),
                      weighting =weightBin))

dim(tdm)

tdm.matrix <- as.matrix(tdm)

tdm.matrix

rownames(tdm.matrix)[1:100]


Sys.getlocale()

localeToCharset()

all.reviews






















































