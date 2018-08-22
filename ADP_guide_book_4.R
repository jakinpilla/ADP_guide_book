# text mining
# install.packages('tm')
setwd("C:/Users/dsc/adp_guidebook")

library(tm)
movie_comment <- ADV_4_5_movieComment_1000
colnames(movie_comment) <- c("num_1", "num_2", "content", "id", "datetime", "num_3", "num_4", "num_5", "before_after")
head(movie_comment)
movie_comment <- movie_comment[ , c(5, 4, 3)]
head(movie_comment)

library(tidyverse)

# install.packages('lubridate')
library(lubridate)
movie_comment$datetime[1]
ymd(movie_comment$datetime[1])
as.Date(movie_comment$datetime[1])
year = substr(movie_comment$datetime, 1, 4)
month = substr(movie_comment$datetime, 6, 7)
day = substr(movie_comment$datetime, 9, 10)

dates = paste(year, month, day, sep='-')
dates <- ymd(dates)

movie <- data.frame(dates, movie_comment)
head(movie)
movie_review <- movie[, c(1, 3, 4)]
head(movie_review)
colnames(movie_review) <- c('date', 'id', 'content')
head(movie_review)
str(movie_review)

write.csv(movie_review, './data/movie_review.csv', fileEncoding = 'utf-8')

movie_review <- read_csv("./data/movie_review.csv")
head(movie_review)

install.packages('rJava')
library(rJava)

install.packages("KoNLP")
library(KoNLP)
install.packages('rJava', .libPaths()[1], 'http://www.rforge.net/')
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_181/bin")

# FP 성장
# 빈발아이템 집합과 연관규칙이라는 두 가지 형태로 관계 표현
# 빈발 정도를 측정하기 위해 지지도의 개념을 연관규칙 관계의 강도를 측정하기 위해 신뢰도의 개념을 사용

# 감정분석

# 사회연결망 분석
# 집합론적인 방법, 그래프 이론에 의한 방법, 행렬을 이용한 방법
# 행과 열에 같은 개체가 배열 :: 원모드 매트릭스
# 행과 열에 다른 개체가 배열 :: 2원모드 매트릭스

# 연결정도 중심성 :: 한 노드에 얼마나 많은 노드들이 관계를 맺고 있는지 기준으로 그 노드가 중심에 위치하는
# 정도를 계량화 한 것

# 근접중심성 :: 각 노드 간의 거리를 근거로 중심성을 측정하는 방법으로, 연결정도 중심성과는 달리 간접적으로 
# 연결된 모든 노드 간의 거리를 합산해 중심성 측정

# 매개중심성 :: 네트워크 내에서 한 노드가 담당하는 매개자 혹은 중재자 역할의 정도로 중심성 측정

# 위세중심성 :: 연결된 노드의 중요성에 가중치를 둬 노드의 중심성을 측정

# 분산처리기술인 하둡 Mapreduce를 활용하거나 하둡 기반의 그래프 프로세싱 프레임워크인 Giraph로 대용량
# 소셜 데이터를 R에서 처리 가능한 수준까지 정제한 후 분석 및 시각화를 수행 가능






































