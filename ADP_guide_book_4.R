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






