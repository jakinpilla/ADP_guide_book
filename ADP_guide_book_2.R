# decison tree
library(rpart)
c <- rpart(Species ~ ., data=iris)
c
plot(c, compress=T, margin = .3)
text(c, cex=1.5)
predict(c, newdata = iris, type='class')

install.packages('rpart.plot')
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

install.packages('party')
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




