library(tidyverse)

# decison tree
library(rpart)
c <- rpart(Species ~ ., data=iris)
c
plot(c, compress=T, margin = .3)
text(c, cex=1.5)
predict(c, newdata = iris, type='class')

# install.packages('rpart.plot')
library(rpart.plot)
prp(c, type=4, extra=2)
ls(c)
c$cptable

opt <- which.min(c$cptable[,'xerror'])
cp <- c$cptable[opt, 'CP']
prune.c <- prune(c, cp=cp)
plot(prune.c)
text(prune.c, use.n=T)

plotcp(c)

# install.packages('party')
library(party)
data("stagec")
str(stagec)

stagec1 <- subset(stagec, !is.na(g2))
stagec2 <- subset(stagec1, !is.na(gleason))
stagec3 <- subset(stagec2, !is.na(eet))
str(stagec3)

set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=T, prob=c(.7, .3))
ind
trainData <- stagec3[ind==1, ]
testData <- stagec3[ind==2, ]
tree <- ctree(ploidy ~., data=trainData)
tree
plot(tree)

testPred = predict(tree, newdata=testData)
testData %>% group_by(ploidy) %>% count()
table(testPred, testData$ploidy)

data("airquality")
airq <- subset(airquality, !is.na(Ozone))
head(airq)
airct <- ctree(Ozone ~., data=airq)
airct
plot(airct)

# 예측값은 최종 마디에 대한 자료들의 평균값
head(predict(airct, data=airq))
# type='node'
predict(airct, data=airq, type='node')
mean((airq$Ozone - predict(airct))^2)

# install.packages('adabag')
library(adabag)
data(iris)
iris.bagging <- bagging(Species ~., data=iris, mfinal=10)
iris.bagging$importance
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

pred <- predict(iris.bagging, newdata = iris)
table(iris.bagging$trees[[10]])

# boosting
boo.adabag <- boosting(Species ~ ., data=iris, boos=T, mfinal=10)
boo.adabag$importance
plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

pred <- predict(boo.adabag, newdata = iris)
tb <- table(pred$class, iris[, 5])

error.rpart <- 1 - (sum(diag(tb))/sum(tb))
error.rpart

# install.packages('ada')
library(ada)
iris[iris$Species != 'setosa', ] -> iris
dim(iris)[1] -> n
n
str(iris)

iris[, 5][2:3]
levels(iris[, 5])[2:3]
as.factor(levels(iris[, 5])[2:3])
as.numeric(iris[, 5]) - 1
as.factor((levels(iris[, 5])[2:3])[as.numeric(iris[, 5]) - 1])
iris[, 5] <- as.factor((levels(iris[, 5])[2:3])[as.numeric(iris[, 5]) - 1]) 

str(iris) # factor 2 levels

# train index(tridx) and test index(teidx)
tridx <- sample(1:n, floor(.6*n), FALSE) # 60% train indice
teidx <- setdiff(1:n, tridx)

# training
gdis <- ada(Species ~., data=iris[trind, ], iter=20, nu=1, type='discrete')

# predicting
gdis <- addtest(gdis, iris[teidx, -5], iris[teidx, 5])
gdis

# kappa plotting
plot(gdis, T, T)

varplot(gdis)
pairs(gdis, iris[trind, -5], maxvar=4)

# RandomForest
# install.packages('randomForest')
library(randomForest)
library(rpart)
data("stagec")
head(stagec)
??stagec # a set of 146 patients with stage C prostate(전립선) cancer
dim(stagec)
str(stagec)
stagec3 <- stagec[complete.cases(stagec), ]
dim(stagec3) # 146 -> 134
str(stagec3)
set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=T, prob=c(.7, .3))
str(ind)
length(ind[ind==1])
length(ind[ind==2])

trainData <- stagec3[ind==1, ]
testData <- stagec3[ind==2, ]
rf <- randomForest(ploidy ~., data=trainData, ntree=100, proximity=T)
table(predict(rf), trainData$ploidy)
print(rf)
plot(rf)
importance(rf)
varImpPlot(rf)
rf.pred <- predict(rf, newdata=testData)
table(rf.pred, testData$ploidy)
plot(margin(rf))

library(party)
set.seed(1234)
cf <- cforest(ploidy ~., data=trainData)
cf.pred <- predict(cf, newdata=testData, OOB=T, type='response')
table(cf.pred, testData$ploidy)

# Holdout
data(iris)
nrow(iris)
set.seed(42)
idx <- sample(2, nrow(iris), replace=T, prob=c(.7, .3))
trainData <- iris[idx==1, ]
testData <- iris[idx==2, ]
nrow(trainData)
nrow(testData)

# cross validation
data(iris)
nrow(iris)
set.seed(42)
k=10
iris <- iris[sample(nrow(iris)), ] # Randomly shuffle the data
folds <- cut(seq(1, nrow(iris)), breaks=k, labels=F)

trainData = list(0)
testData = list(0)

for (i in 1:k) { # Perform 10 fold cross validation
  testIdx <- which(folds==i, arr.ind=T)
  testData[[i]] <- iris[testIdx, ]
  trainData[[i]] <- iris[-testIdx, ]
  }

head(trainData[[1]])
head(testData[[2]])

data(iris)
iris <- subset(iris, Species == 'setosa' | Species == 'versicolor')
iris$Species <- factor(iris$Species)
str(iris)
set.seed(42)
iris <- iris[sample(nrow(iris)), ] # Ramdomly shuffle the data
trainData <- iris[1:nrow(iris)*.7, ]
dim(trainData)
testData <- iris[((nrow(iris)*.7) + 1) : nrow(iris), ]
dim(testData)
nrow(trainData)
nrow(testData)

library(nnet)
library(rpart)
head(trainData)
nn.iris <- nnet(Species ~., data=trainData, size=2, rang=0, decay=5e-4, maxit=200)
dt.iris <- rpart(Species ~., data=trainData)

nn_pred <- predict(nn.iris, testData, type='class')
nn_pred
str(nn_pred)

dt_pred <- predict(dt.iris, testData, type='class')

install.packages('e1071')
library(caret)
nn_pred <- as.factor(nn_pred)
str(testData$Species)
nn_con = confusionMatrix(nn_pred, testData$Species)

































