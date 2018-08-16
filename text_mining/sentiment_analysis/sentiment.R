setwd('C:/Users/dsc/adp_guidebook/text_mining/sentiment_analysis')

mobile <- read.csv('./senti_data/mobile2014.csv', stringsAsFactors = F, encoding = 'utf-8')

library(readr)
mobile <- read_csv('./senti_data/mobile2014.csv')

dim(mobile)
head(mobile)
names(mobile)
mobile[2, ]

mobile[1035, ]
table(mobile$Sentiment)

mobile$Texts <- gsub('[[:punct:]]', '', mobile$Texts)
mobile$Texts <- gsub('[[:cntrl:]]', '', mobile$Texts)
mobile$Texts <- gsub('C訣mara', '', mobile$Texts)

sapply(mobile$Texts[1], function(x) any(x == "C訣mara"))

library(tm)
corpus <- VCorpus(VectorSource(mobile$Texts))
corpus
stopwords()
stopwords("SMART")


dtm <- DocumentTermMatrix(corpus,
                          control = list(tolower = T,
                                         removePunctuation = T,
                                         removeNumbers = T,
                                         stopwords = stopwords("SMART"),
                                         weighting = weightTfIdf))
dtm

library(glmnet)

X <- as.matrix(dtm)
X[1:5, 1:5]
dim(X)

Y <- mobile$Sentiment
Y[1:5]
Y[1001:1005]

res.lm <- glmnet(X, Y, family = "binomial", lambda = 0) # or gaussian
res.lm
coef.lm <- coef(res.lm)[,1]
coef.lm[1:5]

coef.lm > 0
pos.lm <- coef.lm[coef.lm > 0]

coef.lm < 0
neg.lm <- coef.lm[coef.lm < 0]

pos.lm <- sort(pos.lm, decreasing = T)
neg.lm <- sort(neg.lm, decreasing = F)

pos.lm[1:5]
neg.lm[1:5]

length(pos.lm)
length(neg.lm)


## ?Ҽ?�� ǥ??

0.1
.1 = 1/10
.01
.001
1e-04

options(scipen = 1)
1e-04
.00001

options(scipen = 100)
.000000000000000000000000000000000000001
options(scipen = 0)
.001
.00000000001
options()$scipen

# overfitting, lambda understanding
# Lasso(diamond), Ridge(circle)
# Lasso(beta_1 = 0, beta_2)
# Ridge(beta_1, beta_2)


res.lasso <- glmnet(X, Y, family = 'binomial', alpha=1)
res.lasso

plot(res.lasso, xvar = 'lambda')

set.seed(12345)
res.lasso <- cv.glmnet(X, Y, family = 'binomial', alpha = 1,
                       nfolds = 4, type.measure = 'class')
res.lasso

plot(res.lasso)
plot(res.lasso$glmnet.fit, xvar = 'lambda')

coef.lasso <- coef(res.lasso, s = 'lambda.min')[, 1]
coef.lasso

options(scipen = 100)
coef.lasso

pos.lasso <- coef.lasso[coef.lasso > 0]
neg.lasso <- coef.lasso[coef.lasso < 0]

pos.lasso <- sort(pos.lasso, decreasing = T)
neg.lasso <- sort(neg.lasso, decreasing = F)

pos.lasso[1:5]
neg.lasso[1:5]

set.seed(12345)
res.ridge <- cv.glmnet(X, Y, family = 'binomial',
                       alpha = 0, nfolds = 4, 
                       type.measure = 'class')

res.ridge
plot(res.ridge)
plot(res.ridge$glmnet.fit, xvar = 'lambda')

coef.ridge <- coef(res.ridge, s = 'lambda.min')[, 1]
coef.ridge

pos.ridge <- coef.ridge[coef.ridge > 0]
neg.ridge <- coef.ridge[coef.ridge < 0]

pos.ridge <- sort(pos.ridge, decreasing = T)
neg.ridge <- sort(neg.ridge, decreasing = F)

pos.ridge[1:5]
neg.ridge[1:5]

set.seed(12345)
res.elastic <- cv.glmnet(X, Y, family = 'binomial',
                         alpha = .5, nfolds = 4,
                         type.measure = 'class')
res.elastic
plot(res.elastic)
log(0.02424186)
plot(res.elastic$glmnet.fit, xvar = 'lambda')
coef.elastic <- coef(res.elastic, s = 'lambda.min')[,1]
coef.elastic
pos.elastic <- coef.elastic[coef.elastic > 0]
neg.elastic <- coef.elastic[coef.elastic < 0]
pos.elastic <- sort(pos.elastic, decreasing = T)
neg.elastic <- sort(neg.elastic, decreasing = F)

pos.elastic[1:5]
neg.elastic[1:5]

length(pos.lm)
length(neg.lm)

length(pos.lasso)
length(neg.lasso)

length(pos.ridge)
length(neg.ridge)

length(pos.elastic)
length(neg.elastic)

## ??��?ܾ? ???? ??????
write.csv(pos.lm, 'pos.lm.csv')
pos.lm.csv <- read.csv('pos.lm.csv')
pos.lm.csv[1:5, ]

## ??��?ܾ? ???? ??????
write.csv(neg.lm, 'neg.lm.csv')

head(pos.lasso)
write.csv(pos.lasso, 'pos.lasso.csv')
write.csv(neg.lasso, 'neg.lasso.csv')

write.csv(pos.ridge, 'pos.ridge.csv')
write.csv(neg.ridge, 'neg.ridge.csv')

write.csv(pos.elastic, 'pos.elastic.csv')
write.csv(neg.elastic, 'neg.elastic.csv')


# library(devtools) # try install.packages('devtools') if you don't have it installed
# install_github("mannau/tm.plugin.sentiment")
# library(tm.plugin.sentiment) # should work

library(tm.plugin.sentiment)

names(pos.lm)
senti.lm <- polarity(dtm, names(pos.lm), names(neg.lm))
senti.lm

senti.lasso <- polarity(dtm, names(pos.lasso), names(neg.lasso))
senti.ridge <- polarity(dtm, names(pos.ridge), names(neg.ridge))
senti.elastic <- polarity(dtm, names(pos.elastic), names(neg.elastic))

pos.lm.csv <- read.csv('pos.lm.csv')
pos.lm.csv[, 1]

neg.lm.csv <- read.csv('neg.lm.csv')
pos.lasso.csv <- read.csv('pos.lasso.csv')
neg.lasso.csv <- read.csv('neg.lasso.csv')

senti.lm.csv <- polarity(dtm, pos.lm.csv[, 1], neg.lm.csv[, 1])

senti.lm
senti.lm.csv

senti.lasso.csv <- polarity(dtm, pos.lasso.csv[, 1], neg.lasso.csv[, 1])
senti.lasso.csv

##

senti.lm 
senti.lasso
mobile$Sentiment

senti.lm.b <- ifelse(senti.lm > 0, 1, 0)
senti.lasso.b <- ifelse(senti.lasso > 0, 1, 0)
senti.ridge.b <- ifelse(senti.ridge > 0, 1, 0)
senti.elastic.b <- ifelse(senti.elastic > 0, 1, 0)

library(caret)
senti.lm.b
confusionMatrix(senti.lm.b, mobile$Sentiment)
confusionMatrix(senti.lasso.b, mobile$Sentiment)
confusionMatrix(senti.ridge.b, mobile$Sentiment)
confusionMatrix(senti.elastic.b, mobile$Sentiment)

## mobile test

mobile.test <- read.csv('mobile2014_test.csv', stringsAsFactors = FALSE)
dim(mobile.test)
names(mobile.test)
corpus <- VCorpus(VectorSource(mobile.test$Texts))

dtm.test <- DocumentTermMatrix(corpus,
                          control = list(tolower = T,
                                         removePunctuation = T,
                                         removeNumbers = T,
                                         stopwords = stopwords("SMART"),
                                         weighting = weightTfIdf,
                                         dictionary = Terms(dtm)))
dtm.test
dtm

senti.lm.test <- polarity(dtm.test, names(pos.lm), names(neg.lm))
senti.lasso.test <- polarity(dtm.test, names(pos.lasso), names(neg.lasso))
senti.ridge.test <- polarity(dtm.test, names(pos.ridge), names(neg.ridge))
senti.elastic.test <- polarity(dtm.test, names(pos.elastic), names(neg.elastic))

senti.lm.b.test <- ifelse(senti.lm.test > 0, 1, 0)
senti.lasso.b.test <- ifelse(senti.lasso.test > 0, 1, 0)
senti.ridge.b.test <- ifelse(senti.ridge.test > 0, 1, 0)
senti.elastic.b.test <- ifelse(senti.elastic.test > 0, 1, 0)

confusionMatrix(senti.lm.b.test, mobile.test$Sentiment)
confusionMatrix(senti.lasso.b.test, mobile.test$Sentiment)
confusionMatrix(senti.ridge.b.test, mobile.test$Sentiment)
confusionMatrix(senti.elastic.b.test, mobile.test$Sentiment)

lm.acc <- confusionMatrix(senti.lm.b.test, data.test$Sentiment)$overall[1]
lasso.acc <- confusionMatrix(senti.lasso.b.test, data.test$Sentiment)$overall[1]
ridge.acc <- confusionMatrix(senti.ridge.b.test, data.test$Sentiment)$overall[1]
elastic.acc <- confusionMatrix(senti.elastic.b.test, data.test$Sentiment)$overall[1]
acc <- c(lm.acc, lasso.acc, ridge.acc, elastic.acc)
names(acc) <- c('lm', 'lasso', 'ridge', 'elastic')

mobile.acc <- acc
mobile.acc

## nfolds = 4 : train 3, test 1?? ??��

## test(tablet)

data.test <- read.csv('tablet2014_test.csv',  stringsAsFactors = F)

corpus <- VCorpus(VectorSource(data.test$Texts))

dtm.test <- DocumentTermMatrix(corpus,
                               control = list(tolower = T,
                                              removePunctuation = T,
                                              removeNumbers = T,
                                              stopwords = stopwords("SMART"),
                                              weighting = weightTfIdf,
                                              dictionary = Terms(dtm)))

library(tm.plugin.sentiment)

senti.lm.test <- polarity(dtm.test, names(pos.lm), names(neg.lm))
senti.lasso.test <- polarity(dtm.test, names(pos.lasso), names(neg.lasso))
senti.ridge.test <- polarity(dtm.test, names(pos.ridge), names(neg.ridge))
senti.elastic.test <- polarity(dtm.test, names(pos.elastic), names(neg.elastic))

senti.lm.b.test <- ifelse(senti.lm.test > 0, 1, 0)
senti.lasso.b.test <- ifelse(senti.lasso.test > 0, 1, 0)
senti.ridge.b.test <- ifelse(senti.ridge.test > 0, 1, 0)
senti.elastic.b.test <- ifelse(senti.elastic.test > 0, 1, 0)

library(caret)

confusionMatrix(senti.lm.b.test, data.test$Sentiment)
confusionMatrix(senti.lasso.b.test, data.test$Sentiment)
confusionMatrix(senti.ridge.b.test, data.test$Sentiment)
confusionMatrix(senti.elastic.b.test, data.test$Sentiment)

lm.acc <- confusionMatrix(senti.lm.b.test, data.test$Sentiment)$overall[1]
lasso.acc <- confusionMatrix(senti.lasso.b.test, data.test$Sentiment)$overall[1]
ridge.acc <- confusionMatrix(senti.ridge.b.test, data.test$Sentiment)$overall[1]
elastic.acc <- confusionMatrix(senti.elastic.b.test, data.test$Sentiment)$overall[1]
acc <- c(lm.acc, lasso.acc, ridge.acc, elastic.acc)
names(acc) <- c('lm', 'lasso', 'ridge', 'elastic')

tablet.acc <- acc
tablet.acc

## test(books)

data.test <- read.csv('books_test.csv',  stringsAsFactors = F)

corpus <- VCorpus(VectorSource(data.test$Texts))

dtm.test <- DocumentTermMatrix(corpus,
                               control = list(tolower = T,
                                              removePunctuation = T,
                                              removeNumbers = T,
                                              stopwords = stopwords("SMART"),
                                              weighting = weightTfIdf,
                                              dictionary = Terms(dtm)))

library(tm.plugin.sentiment)

senti.lm.test <- polarity(dtm.test, names(pos.lm), names(neg.lm))
senti.lasso.test <- polarity(dtm.test, names(pos.lasso), names(neg.lasso))
senti.ridge.test <- polarity(dtm.test, names(pos.ridge), names(neg.ridge))
senti.elastic.test <- polarity(dtm.test, names(pos.elastic), names(neg.elastic))

senti.lm.b.test <- ifelse(senti.lm.test > 0, 1, 0)
senti.lasso.b.test <- ifelse(senti.lasso.test > 0, 1, 0)
senti.ridge.b.test <- ifelse(senti.ridge.test > 0, 1, 0)
senti.elastic.b.test <- ifelse(senti.elastic.test > 0, 1, 0)

library(caret)

confusionMatrix(senti.lm.b.test, data.test$Sentiment)
confusionMatrix(senti.lasso.b.test, data.test$Sentiment)
confusionMatrix(senti.ridge.b.test, data.test$Sentiment)
confusionMatrix(senti.elastic.b.test, data.test$Sentiment)

lm.acc <- confusionMatrix(senti.lm.b.test, data.test$Sentiment)$overall[1]
lasso.acc <- confusionMatrix(senti.lasso.b.test, data.test$Sentiment)$overall[1]
ridge.acc <- confusionMatrix(senti.ridge.b.test, data.test$Sentiment)$overall[1]
elastic.acc <- confusionMatrix(senti.elastic.b.test, data.test$Sentiment)$overall[1]
acc <- c(lm.acc, lasso.acc, ridge.acc, elastic.acc)
names(acc) <- c('lm', 'lasso', 'ridge', 'elastic')

books.acc <- acc
books.acc

mobile.acc
tablet.acc
books.acc

## ??��?????? ȸ?Ͱ????? ?̿??? ??��?м?

data.test <- read.csv('tablet2014_test.csv', stringsAsFactors = F)

corpus <- VCorpus(VectorSource(data.test$Texts))
dtm.test <- DocumentTermMatrix(corpus,
                               control = list(tolower = T,
                                              removePunctuation = T,
                                              removeNumbers = T,
                                              stopwords = stopwords("SMART"),
                                              weighting = weightTfIdf,
                                              dictionary = Terms(dtm)))

X.test <- as.matrix(dtm.test)
senti.lm.test.coef <- predict(res.lm , newx = X.test)
senti.lasso.test.coef <- predict(res.lasso, newx = X.test, s = "lambda.min")
senti.ridge.test.coef <- predict(res.ridge, newx = X.test, s = "lambda.min")
senti.elastic.test.coef <- predict(res.elastic, newx = X.test, s = "lambda.min")

senti.lm.test.coef
senti.lasso.test.coef
senti.ridge.test.coef
senti.elastic.test.coef

senti.lm.b.test.coef <- ifelse(senti.lm.test.coef > 0, 1, 0)
senti.lasso.b.test.coef <- ifelse(senti.lasso.test.coef > 0, 1, 0)
senti.ridge.b.test.coef <- ifelse(senti.ridge.test.coef > 0, 1, 0)
senti.elastic.b.test.coef <- ifelse(senti.elastic.test.coef > 0, 1, 0)

confusionMatrix(senti.lm.b.test.coef, data.test$Sentiment)$overall[1]
confusionMatrix(senti.lasso.b.test.coef, data.test$Sentiment)$overall[1]
confusionMatrix(senti.ridge.b.test.coef, data.test$Sentiment)$overall[1]
confusionMatrix(senti.elastic.b.test.coef, data.test$Sentiment)$overall[1]










