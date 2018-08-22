library(tidyverse)

# decison tree
library(rpart)
c <- rpart(Species ~ ., data=iris)
c
plot(c, compress=T, margin = .3)
text(c, cex=1.5)
predict(c, newdata = iris, type='class')
ㅗㄷㅁㅇpredict(c, newdata = iris, type='class')
# 지니지수, 엔트로피지수, F-통계량의 p_value
# F-통계량은 일원배치법에서의 검정통계량으로 그 값이 클수록 오차의 변동에 비해 처리(treatment) 변동이 큼을 의미
# 이는 자식노드 간 이질적임을 의미하므로 이 값이 커지는 방향(p-value 가 작아지는 방향)으로 가지분할을 수행ㅊ
# 분산의 감소량(variance reduction)도 이 값이 최대화 되는 방향으로 가지분할을 수행

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

# train data와 test data를 7:3으로 나눔
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
# Ozone
# [1,] 18.47916667
# [2,] 18.47916667
# [3,] 18.47916667
# [4,] 18.47916667
# [5,] 18.47916667
# [6,] 18.47916667

# type='node'
predict(airct, data=airq, type='node')
mean((airq$Ozone - predict(airct))^2)

# 의사결정나무의 장단점
# 유용한 입력변수의 파악과 예측변수간의 상호작용 및 비선형성을 고려하여 분석 수행
# 선형성, 정규성, 등분산성 등의 수학적 가정이 불필요한 비모수적 모형
# 분류기준값의 경계선 근방의 자료값에 대해서는 오차가 큼(비연속성)
# 로지스틱 회귀처럼 각 예측변수의 효과를 파악하기 곤란
# 새로운 자료에 대한 예측 불안정

# install.packages('adabag')
library(adabag)
data(iris)
iris.bagging <- bagging(Species ~., data=iris, mfinal=10)
iris.bagging$importance
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

pred <- predict(iris.bagging, newdata = iris)
table(pred$class, iris[,5])

# boosting
boo.adabag <- boosting(Species ~ ., data=iris, boos=T, mfinal=10)
boo.adabag$importance
plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

pred <- predict(boo.adabag, newdata = iris)
tb <- table(pred$class, iris[, 5])
tb

error.rpart <- 1 - (sum(diag(tb))/sum(tb))
error.rpart

# install.packages('ada')
library(ada)
iris[iris$Species != 'setosa', ] -> iris # setosa 제외
dim(iris)[1] -> n
n


iris[, 5][2:3]
levels(iris[, 5])[2:3]
as.factor(levels(iris[, 5])[2:3])
as.numeric(iris[, 5]) - 1

as.factor(levels(iris[, 5])[2:3])[as.numeric(iris[, 5]) - 1]
iris[, 5] <- as.factor((levels(iris[, 5])[2:3])[as.numeric(iris[, 5]) - 1]) 

str(iris) # factor 2 levels
# 'data.frame':	100 obs. of  5 variables:
# $ Sepal.Length: num  7 6.4 6.9 5.5 6.5 5.7 6.3 4.9 6.6 5.2 ...
# $ Sepal.Width : num  3.2 3.2 3.1 2.3 2.8 2.8 3.3 2.4 2.9 2.7 ...
# $ Petal.Length: num  4.7 4.5 4.9 4 4.6 4.5 4.7 3.3 4.6 3.9 ...
# $ Petal.Width : num  1.4 1.5 1.5 1.3 1.5 1.3 1.6 1 1.3 1.4 ...
# $ Species     : Factor w/ 2 levels "versicolor","virginica": 1 1 1 1 1 1 1 1 1 1 ...

# train index(tridx) and test index(teidx)
tridx <- sample(1:n, floor(.6*n), FALSE) # 60% train indice
teidx <- setdiff(1:n, tridx)

# training
gdis <- ada(Species ~., data=iris[tridx, ], iter=20, nu=1, type='discrete')

# predicting
gdis <- addtest(gdis, iris[teidx, -5], iris[teidx, 5])
gdis

# kappa plotting
plot(gdis, T, T)

# kappa :: 두 관찰자 사이의 일치도 확인
# Pa :: 2명의 평가자간 일치확률
# Pc :: 우연히 두 평가자에 의해 일치된 평가를 받을 확률
# kappa = (Pa - Pc) / (1 - Pc)

varplot(gdis)
pairs(gdis, iris[tridx, -5], maxvar=4)

# RandomForest
# install.packages('randomForest')
library(randomForest)
library(rpart)
data("stagec")
head(stagec)
??stagec # a set of 146 patients with stage C prostate(전립선) cancer
dim(stagec)
str(stagec)

# remove NA
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
# 오류율에 대한 OOB(out-of-bag) 추정치 제공(3.92%)
# 랜덤포레스트에서는 별도의 검증용 데이터를 사용하지 않더라도 붓스트랩 샘플과정에서 제외된 자료를
# 사용하여 검증 실시 가능

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

# 모형평가 방법 : 홀드아웃, 교차검증, 붓스트램

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

head(trainData[[1]]) # 첫 번째 fold
head(testData[[2]]) # 두 번째 fold

data(iris)
iris <- subset(iris, Species == 'setosa' | Species == 'versicolor')
iris$Species <- factor(iris$Species)
str(iris)
set.seed(42)
iris <- iris[sample(nrow(iris)), ] # Ramdomly shuffle the data

# 이미 shuffled 되어 있으므로...
trainData <- iris[1:nrow(iris)*.7, ]
dim(trainData)
head(trainData)
testData <- iris[((nrow(iris)*.7) + 1) : nrow(iris), ]
dim(testData)
nrow(trainData)
nrow(testData)

# d개의 관측치가 있는 데이터가 있을 때 각 관측치가 훈련용 자료로 선정될 확률은 1/d이며 선정되지
# 않은 확률은 (1-1/d) 이다. 따라서 훈련용 자료의 선정을 d번 반복할 때 하나의 관측치가 선정되지 않을
# 확률은 (1-1/d)d 이며 d가 크다고 가정할 때의 확률은 e-1=.368로 수렴한다.
# 36.8%의 관측치는 훈련용 집합으로 선정되지 않아 검증용 자료로 사용되며 나머지 63.2%의 관측치가 
# 훈련용 자료로 사용됨

library(nnet)
library(rpart)
head(trainData)
nn.iris <- nnet(Species ~., data=trainData, size=2, rang=0, decay=5e-4, maxit=200)
dt.iris <- rpart(Species ~., data=trainData)

nn_pred <- predict(nn.iris, testData, type='class')
nn_pred
str(nn_pred)

dt_pred <- predict(dt.iris, testData, type='class')

# install.packages('e1071')
library(e1071)
library(caret)
nn_pred <- as.factor(nn_pred)
str(testData$Species)
nn_con = confusionMatrix(nn_pred, testData$Species)
nn_con$table

dt_con = confusionMatrix(dt_pred, testData$Species)
dt_con
dt_con$table

nn_con$overall['Accuracy']
dt_con$overall['Accuracy']

nn_con$byClass['Sensitivity']
dt_con$byClass['Sensitivity']

accuracy <- c(nn_con$overall['Accuracy'], dt_con$overall['Accuracy'])
precision <- c(nn_con$byClass['Pos Pred Value'], dt_con$byClass['Pos Pred Value'])
recall <- c(nn_con$byClass['Sensitivity'], dt_con$byClass['Sensitivity'])
f1 <- 2*(precision*recall) / (precision + recall)
result <- data.frame(rbind(accuracy, precision, recall, f1))
names(result) <- c('nnet', 'decision tree')
result

data(infert)
head(infert)
# parity : the number of times a female has given birth
# case :case status (1 : case, 0 :controlled)
# stratum : ??
# pooled.stratum : ??
tail(infert)

# ROC graph
set.seed(42)
infert <- infert[sample(nrow(infert)), ] # suffling
infert <- infert[, c('age', 'parity', 'induced', 'spontaneous', 'case')]
head(infert)
nrow(infert)

trainData <- infert[1:(nrow(infert)*.7), ]
testData <- infert[((nrow(infert)*.7 + 1) : nrow(infert)), ]
nrow(trainData)
nrow(testData)

# neural network
library(neuralnet)
net.infert <- neuralnet(case ~ age+parity+induced+spontaneous, data=trainData, hidden=3, 
                        err.fct='ce', linear.output=F, likelihood=T)

n_test <- subset(testData, select=-case)

nn_pred <- neuralnet::compute(net.infert, n_test)
testData$net_pred <- nn_pred$net.result
head(testData)

# decison tree
# install.packages('C50')
library(C50)
trainData$case <- factor(trainData$case)
dt.infert <- C50::C5.0(case ~ age + parity + induced + spontaneous, data=trainData)
testData$dt_pred <- predict(dt.infert, testData, type='prob')[, 2]

head(testData)

# install.packages('Epi')
library(Epi)
neural_ROC <- ROC(form=case ~ net_pred, data = testData, plot='ROC')
dtree_ROC <- ROC(form=case ~ dt_pred, data = testData, plot='ROC')

## 이익도표에 대한 생각

head(testData)
# net_pred가 높은 순으로 정렬
gain_tbl <- testData[order(testData$net_pred, decreasing = T), ][, c('case', 'net_pred')]
head(gain_tbl, 10)
str(gain_tbl)
gain_tbl$net_pred <- as.numeric(gain_tbl$net_pred)

gain_tbl %>% 
  select(case, net_pred) %>%
  mutate(group = cut(net_pred, 
                     breaks = seq(0, 1, .1),
                     include.lowest = T, # 0을 그룹에 포함시키기 위해 반드시 필요, 아니면 NA값 반환됨.
                     labels=c('0-10', '10-20', '20-30', '30-40', '40-50',
                              '50-60', '60-70', '70-80', '80-90', '90-100')))

nrow(testData) # 74
nrow(subset(testData, testData$case == 1)) # 28
quanted <- quantile(gain_tbl$net_pred, seq(0,1,.1))
str(quanted)
quanted['90%']
unname(quanted['90%'])

transformed <- transform(gain_tbl, 
                         group=cut(net_pred, breaks = quanted, include.lowest = T,
                                   labels=c('0-10', '10-20', '20-30', '30-40', '40-50',
                                            '50-60', '60-70', '70-80', '80-90', '90-100')))

str(transformed)
transformed %>% group_by(group) %>% summarise(sum.case=sum(case)) %>% arrange(desc(group))

# base lift
28/74

# install.packages('ROCR')
library(ROCR)
str(testData)
testData$net_pred <- as.numeric(testData$net_pred )
n_r <- prediction(testData$net_pred, testData$case)
d_r <- prediction(testData$dt_pred, testData$case)
n_p <- performance(n_r, 'tpr', 'fpr')
d_p <- performance(d_r, 'tpr', 'fpr')
plot(n_p, col='red')
par(new=T)
plot(d_p, col='blue')
abline(a=0, b=1)
n_lift <- performance(n_r, 'lift', 'rpp')
plot(n_lift, col='red')
abline(v=.2)










