library(tidyverse)

# decison tree
library(rpart)

c <- rpart(Species ~ ., data=iris)
c
plot(c, compress=T, margin = .3)
text(c, cex=1.5)
predict(c, newdata = iris, type='class')

table(iris$Species, predict(c, newdata = iris, type='class'))

# 지니지수, 엔트로피지수, F-통계량의 p_value
# F-통계량은 일원배치법에서의 검정통계량으로 그 값이 클수록 오차의 변동에 비해 처리(treatment) 변동이 큼을 의미
# 이는 자식노드 간 이질적임을 의미하므로 이 값이 커지는 방향(p-value 가 작아지는 방향)으로 가지분할을 수행한다.
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

# https://www.crocus.co.kr/1283 
# Post-pruning
# 먼저 tree 를 완전히 자라게 한 다음 Bottom-up 방식으로 tree 를 가지 치기하는 방식
# Reduced Error Pruning:  가지치기를 시도해 보고  만일 generalization error가 개선 된다면 subtree를 단말 node 로 대치 함.
# 
# rpart 에서는 error 계산을 위하여 training data set으로 X(cross) validation한다.
# 단말 node 의 class label은 node에서 다수를 차지하는 record의 class label로 정함. 
# 이때 rpart 패키지에서는 cp값을 증가시켜가며 tree 크기를 감소시켜 x validation error(xerror)을 계산한다.
# 이때 xerror이 최소로 하는 cp가 최적이다.

# -------------------------------------------------------------------------

# install.packages('party')
library(party)
data("stagec")
str(stagec)

library(Amelia)
missmap(stagec)

stagec1 <- subset(stagec, !is.na(g2))
stagec2 <- subset(stagec1, !is.na(gleason))
stagec3 <- subset(stagec2, !is.na(eet))
str(stagec3)
nrow(stagec3)

stagec4 <- na.omit(stagec)
nrow(stagec4)

# train data와 test data를 8:2으로 나눔
set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=T, prob=c(.7, .3))
ind

trainData <- stagec3[ind==1, ]
testData <- stagec3[ind==2, ]

## --

# ploidy : 배수성 / 정상

# tetraploidy : an extremely rare chromosomal anomaly, polyploidy, when an affected individual has four copies of each chromosome, instead of two, resulting in total of 92 chromosomes in each cell. 

library(rsample)
?initial_split
data_split <- initial_split(stagec3, .8)
train_data <- training(data_split)
test_data <- testing(data_split)

train_data %>% dim()
test_data %>% dim()

tree <- ctree(ploidy ~., data=train_data)
tree
plot(tree)

test_pred = predict(tree, newdata=test_data)
test_data %>% group_by(ploidy) %>% count()
table(test_pred, test_data$ploidy)


# -------------------------------------------------------------------------

# 동 알고리즘은 반응변수가 연속형인 경우, 의사결정나무(회귀나무)를 통한 예측을 수행한다.

data("airquality")
Amelia::missmap(airquality)
nrow(airquality)

airq <- subset(airquality, !is.na(Ozone))
nrow(airq)
head(airq)

airq2 <- na.omit(airquality)
nrow(airq2)

airct <- ctree(Ozone ~., data=airq)
airct
plot(airct)

# 예측값은 최종 마디에 대한 자료들의 평균값이다.
head(predict(airct, data=airq))
# Ozone
# [1,] 18.47916667
# [2,] 18.47916667
# [3,] 18.47916667
# [4,] 18.47916667
# [5,] 18.47916667
# [6,] 18.47916667

# type='node'
yhat_ctree <- predict(airct, data=airq, type='node')
yhat_ctree %>% table()
df_1 <- yhat_ctree %>% table() %>% enframe()

yhat_ctree_not_node <- predict(airct, data=airq) %>% round(2)
yhat_ctree_not_node %>% table() 
df_2 <- yhat_ctree_not_node %>% table() %>% enframe()

df_2 %>% 
  dplyr::rename(mean.value_per_node = name) -> df_3

df_1 %>%
  left_join(df_3)

mean((airq$Ozone - predict(airct))^2)


# 의사결정나무의 장단점
# 유용한 입력변수의 파악과 예측변수간의 상호작용 및 비선형성을 고려하여 분석 수행
# 선형성, 정규성, 등분산성 등의 수학적 가정이 불필요한 비모수적 모형
# 분류기준값의 경계선 근방의 자료값에 대해서는 오차가 큼(비연속성)
# 로지스틱 회귀처럼 각 예측변수의 효과를 파악하기 곤란
# 새로운 자료에 대한 예측 불안정

# 고객 타겟팅, 고객들의 신용점수화, 캠페인 반응분석, 고객행동예측, 고객 세분화 등에 활용

# 앙상블 -------------------------------------------------------------------------

# Bagging...
# install.packages('adabag')
library(adabag)
data(iris)
iris.bagging <- bagging(Species ~., data=iris, mfinal=10)
iris.bagging$importance

# Petal.Length > Petal.Width > Sepal.Length > Sepal.Width...
par(mfrow = c(1, 1))
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

pred <- predict(iris.bagging, newdata = iris)
table(pred$class, iris[,5])

# -------------------------------------------------------------------------

# Boosting....
# 부트스트랩 표본을 구성하는 재표본 과정에서 각 자료에 동일한 확률을 부여하는 것이
# 아니라 분류가 잘못된 데이터에  더 큰 가중을 주어 표본을 추출한다. 부스팅에서는
# 붓스트랩 표본을 추출하여 분류기를 만든 후 그 분류결과를 이용하여 각 데이터가 
# 추출될 확률을 조정한 후 다음 붓스트랩 표본을 추출하는 과정을 반복한다.

boo.adabag <- boosting(Species ~ ., data=iris, boos=T, mfinal=10)
boo.adabag$importance

# Petal.Length >  Petal.Width > Sepal.Length  > Sepal.Width

plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

pred <- predict(boo.adabag, newdata = iris)
tb <- table(pred$class, iris[, 5])
tb

error.rpart <- 1 - (sum(diag(tb))/sum(tb))
error.rpart

# -------------------------------------------------------------------------

# install.packages('ada')
library(ada)
# iris[iris$Species != 'setosa', ] -> iris # setosa 제외

iris %>% nrow()
iris %>%
  filter(Species %in% c('versicolor', 'virginica')) -> iris_1

# dim(iris_1)[1] -> n
nrow(iris_1) -> n 
n


# iris_1[, 5][2:3]
# levels(iris_1[, 5])[2:3]
# as.factor(levels(iris_1[, 5])[2:3])
# as.numeric(iris_1[, 5]) - 1
# 
# as.factor(levels(iris_1[, 5])[2:3])[as.numeric(iris_1[, 5]) - 1]
# iris_1[, 5] <- as.factor((levels(iris_1[, 5])[2:3])[as.numeric(iris_1[, 5]) - 1]) 
# 
# str(iris_1) # factor 2 levels
# 'data.frame':	100 obs. of  5 variables:
# $ Sepal.Length: num  7 6.4 6.9 5.5 6.5 5.7 6.3 4.9 6.6 5.2 ...
# $ Sepal.Width : num  3.2 3.2 3.1 2.3 2.8 2.8 3.3 2.4 2.9 2.7 ...
# $ Petal.Length: num  4.7 4.5 4.9 4 4.6 4.5 4.7 3.3 4.6 3.9 ...
# $ Petal.Width : num  1.4 1.5 1.5 1.3 1.5 1.3 1.6 1 1.3 1.4 ...
# $ Species     : Factor w/ 2 levels "versicolor","virginica": 1 1 1 1 1 1 1 1 1 1 ...


iris_1$Species <- factor(iris_1$Species, levels = c('versicolor', 'virginica'), labels = c(0, 1))
iris_1 %>% str()

# train index(tridx) and test index(teidx)
tridx <- sample(1:n, floor(.6*n), FALSE) # 60% train indice
teidx <- setdiff(1:n, tridx)

library(rsample)

data_split <- initial_split(iris_1, .6)
train_data <- data_split %>% training 
train_data %>% nrow()
train_data$Species %>% table()

test_data <- data_split %>% testing
test_data %>% nrow()
test_data$Species %>% table()

# training
gdis <- ada(Species ~., data=train_data, iter=20, nu=1, type='discrete')

# predicting
gdis <- addtest(gdis, test_data[, -5], test_data[, 5])
gdis

# kappa plotting
par(mfrow = c(1,1))
plot(gdis, T, T)

# kappa :: 두 관찰자 사이의 일치도 확인
# Pa :: 2명의 평가자간 일치확률
# Pc :: 우연히 두 평가자에 의해 일치된 평가를 받을 확률
# kappa = (Pa - Pc) / (1 - Pc)

varplot(gdis)
pairs(gdis, test_data[, -5], maxvar=4)

# apply ada method to breast cancer dataset...

# loading data from my local mysql db...

library(DBI)
library(RMySQL)
con <- dbConnect(
  MySQL(),
  user = "root", 
  password = 'chr0n3!7!',
  dbname = "breast_cancer",
)

data <- dbGetQuery(con, "select * from breast_cancer;")


# breast cancer data EDA...

data %>% glimpse()
summary(data)

data %>%
  dplyr::select(-id, -row_names) %>%
  mutate(class = recode(class, 
                        'M'= 1,
                        'B' = 0)) -> data_1

data_1 %>% colnames()

data_1 %>% dim()


library(psych)
# pairs.panels(data_1) # time consumming because of many columns...

data_1 %>% 
  dplyr::select(class, contains('mean_')) %>%
  pairs.panels()


data_1 %>% 
  dplyr::select(class, contains('se_')) %>%
  pairs.panels()


data_1 %>% 
  dplyr::select(class, contains('worst_')) %>%
  pairs.panels()

# which variables are correlated with the class variable?...

cor(data_1) %>%
  as_tibble() -> data_2

data_2 %>%
  mutate(row_name = colnames(.)) %>%
  dplyr::select(row_name, everything()) -> data_3

data_3 %>%
  filter(row_name == 'class') %>%
  gather(variable, value, -row_name) %>%
  arrange(desc(value)) -> data_4
  
data_4$variable <- factor(data_4$variable, levels = rev(data_4$variable))
data_4 %>% str()

data_4 %>%
  slice(-1) %>%
  ggplot(aes(variable, value)) + geom_point() + coord_flip()

# worst_concave_points, mean_perimeter, worst_area, mean_concavity....

# data visualizing...

data$class <- as.factor(data$class)

library(gridExtra)

data %>% 
  dplyr::select(class, worst_concave_points) %>%
  ggplot(aes(class, worst_concave_points, col = class)) +
  geom_boxplot() +
  geom_jitter(alpha = .5) +
  scale_color_brewer(palette = "Set1") -> p1


data %>% 
  dplyr::select(class, mean_perimeter) %>%
  ggplot(aes(class, mean_perimeter, col = class)) +
  geom_boxplot() +
  geom_jitter(alpha = .5) +
  scale_color_brewer(palette = "Set1") -> p2


data %>% 
  ggplot(aes(worst_concave_points, mean_perimeter, col = class)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") -> p3

data_n <- data %>% count(class)

data_n %>%
  ggplot(aes(class, n, fill = class)) + geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = "Set1") -> p4

grid.arrange(p1, p2, p3, p4, ncol = 2)

# data spliting to apply ada method...

library(caret)

train_idx <- createdata_1Partition(data_1$class, p = .8, list = F)[, 1]
train_idx %>% length()

test_idx <- setdiff(1:nrow(data_1_1), train_idx)
test_idx %>% length()

train_set <- data_1[train_idx, ]
test_set <- data_1[test_idx, ]

train_set %>% dim()
test_set %>% dim()

train_set %>% str()
test_set %>% str()

train_set$class <- factor(train_set$class, levels = c(0, 1), labels = c(0, 1))
test_set$class <- factor(test_set$class, levels = c(0, 1), labels = c(0, 1))

train_set %>% distinct(class)
test_set %>% distinct(class)

# modeling with ada...

m <- ada(class ~., data=train_set, iter=20, nu=1)
m


# validation -------------------------------------------------------------------------
addtest(m, test_set[, -1], test_set[, 1])

yhat_ada <- predict(m, test_set[, -1], type = 'prob')[, 2]
yobs <- test_set[, 1]

library(ROCR)
y_pred_ada <- prediction(yhat_ada, yobs)
y_perf_ada <- performance(y_pred_ada, measure = 'tpr', x.measure = 'fpr')

plot(y_perf_ada)
abline(0, 1)

performance(y_pred_ada, 'auc')@y.values[[1]]
# 0.9850771

test_set[, 1] %>% 
  enframe() %>% mutate(y_hat = round(yhat_ada, 2)) # %>% View



# -------------------------------------------------------------------------
yobs <- test_set[, 1]
m_ada_boost <- boosting(class ~ ., data=train_set, boos=T, mfinal=10)
yhat_ada_boost_obs <- predict(m_ada_boost, test_set[, -1], type = 'prob')
yhat_ada_boost <- yhat_ada_boost_obs$prob[, 2]

y_pred_ada_boost <- prediction(yhat_ada_boost, yobs)
y_perf_ada_boost <- performance(y_pred_ada_boost, measure = 'tpr', x.measure = 'fpr')

plot(y_perf_ada_boost)
abline(0, 1)

performance(y_pred_ada_boost, 'auc')@y.values[[1]]
# 0.9785379

test_set[, 1] %>% 
  enframe() %>% mutate(yhat_ada = round(yhat_ada, 2)) %>%
  mutate(yhat_ada_boost = round(yhat_ada_boost, 2))


plot(y_perf_ada)
plot(y_perf_ada_boost, add = T, col = 'red', lty = 2)
abline(0, 1)


# RandomForest
# install.packages('randomForest')
library(randomForest)
library(rpart)
data("stagec")
head(stagec)
??stagec # a set of 146 patients with stage C prostate(전립선) cancer
dim(stagec)
str(stagec)

# Random Forest-------------------------------------------------------------------------

# 새로운 자료에 대한 예측은 분류의 경우는 다수결로, 회귀의 경우에는 평균을 취하는 방법을 사용

# remove NA
stagec3 <- stagec[complete.cases(stagec), ]
dim(stagec3) # 146 -> 134
str(stagec3)
set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=T, prob=c(.7, .3))
str(ind)
length(ind[ind==1])
length(ind[ind==2])

library(rsample)
initial_split(stagec3, .7) -> data_split

data_split %>% training() -> trainData
data_split %>% testing() -> testData

# install.packages('randomForest')
library(randomForest)
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

# 훈련용 자료값의 마진: 마진은 랜덤포레스트 분류기 가운데 정분류를 수행한 비율에서
# 다른 클래스로 분류한 비율의 최대치를 뺀 값을 나타낸다. 즉, 양의 마진은 정확한 분류를 
# 의미하며 음은 그 반대이다.

plot(margin(rf))

# -------------------------------------------------------------------------

library(party)
set.seed(1234)
cf <- cforest(ploidy ~., data=trainData)
cf.pred <- predict(cf, newdata=testData, OOB=T, type='response')
table(cf.pred, testData$ploidy)

# 모형평가 방법 : 홀드아웃, 교차검증, 붓스트램-----------------------------

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


# -------------------------------------------------------------------------

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

names(net.infert)

net.infert$result.matrix

# 모형 적합에 사용된 자료는 $covariater과 $response를 통해 확인 가능하다
# 적합값은 $net.result에 제공된다.

out <- cbind(net.infert$covariate, net.infert$net.result[[1]])
out %>% dim()
dimnames(out) <- list(NULL, c("age", "parity", "induced", "spontaneous", "nn-output"))

head(out)

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
# library(Epi)

# install.packages('cmprsk')
# library(cmprsk)

# neural_ROC <- ROC(form=case ~ net_pred, data = testData, plot='ROC')
# dtree_ROC <- ROC(form=case ~ dt_pred, data = testData, plot='ROC')

## ROC Curve...
library(ROCR)
nn_pred <- neuralnet::compute(net.infert, n_test)
yhat_nn <- nn_pred$net.result
y_obs <- testData$case

pred_nn <- prediction(yhat_nn, y_obs)
perf_nn <- performance(pred_nn, measure = 'tpr', x.measure = 'fpr')
plot(perf_nn, col = 'green')
abline(0, 1)


yhat_dt <- predict(dt.infert, testData, type = 'prob')[, 2]
pred_dt <- prediction(yhat_dt, y_obs)
perf_dt <- performance(pred_dt, measure= 'tpr', x.measure = 'fpr')
plot(perf_dt, col = 'red')
abline(0, 1)


plot(perf_nn, col = 'green')
plot(perf_dt, add = T, col= 'red')
abline(0, 1)


## 이익도표에 대한 생각

head(testData)
nrow(testData)

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











