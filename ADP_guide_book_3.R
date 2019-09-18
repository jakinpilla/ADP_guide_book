setwd("~/ADP_guide_book")
# clustering :: 계층적군집, 분리군집, 밀도기반군집, 모형기반군집, 격자기반군집, 커널기반군집, SOM
# 최단연결법(단일연결법), 최장연결법(완전연결법), 중심연결법, 평균연결법, 와드연결법
# 와드연결법 :: 군집 내 오차제곱합에 기초하여 군집 수행, 보통 두 군집이 합해지면 병합된 군집의 오차제곱합은
# 병합이전보다 커짐, 그 증가량이 작아지는 방향으로 군집이 생성됨. 와드연결법은 크기가 비슷한 군집끼리 병합하는
# 경향이 있음

# 유클리드 거리
# 맨하튼 거리
# 민코우스키 거리
# 표준화거리:: (유클리드거리/표준편차), 단위 및 분산의 차이로 인한 왜곡 회피 가능, 표본분산 대각행렬
# 마할라노비스 거리 :: 표준편차와 변수간 상관관계까지 고려한 거리, 표본공분산 행렬
# 쳬비셰프 거리 :: max|Xi - Yi|

data("USArrests")
str(USArrests)

d <- dist(USArrests, method='euclidean')
fit <- hclust(d, method='ave') # 평균연결법

par(mfrow=c(1,2))
plot(fit)
plot(fit, hang=-1)
par(mfrow=c(1,1))

groups <- cutree(fit, k=6)    
groups

plot(fit)

rect.hclust(fit, k=6, border='red')


hca <- hclust(dist(USArrests))
plot(hca)
rect.hclust(hca, k=3, border='red')
rect.hclust(hca, h=50, which=c(2, 7), border=3:4) # h는 높이, which는 위치

# agnes() 함수는 계층적 군집방법 중 병합적 방법을 이용
# daisy() 함수는 데이터 관측치 사이의 거리를 계산해 주며, 자료의 형태가 수치형일 필요가 없음

library(cluster)
agn1 <- agnes(USArrests, metric='manhattan', stand=T)
agn1
par(mfrow=c(1, 2))
plot(agn1)

agn2 <- agnes(daisy(USArrests), diss=T, method='complete')
plot(agn2)

agn3 <- agnes(USArrests, method='flexible', par.meth=.6)
plot(agn3)
par(mfrow=c(1, 1))

# K-means
# 탐욕적 알고리즘
# 잡음이나 이상값에 영향을 많이 받음
# 볼록한 형태가 아닌 군집이 존재할 경우 성능 저하됨
# k-medoid :: k 중앙값군집은 pam() 함수를 이용

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for ( i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type='b', xtaba='Number of Clusters', 
       ylab = 'Within groups sum of squares')
}

# install.packages('rattle')
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine <- read.csv(url)

head(wine)
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash',
                    'Alcalinity', 'Magnesium', 'Phenols',
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue',
                    'Dilution', 'Proline')

wine$Type <- as.factor(wine$Type)
head(wine)

write.csv(wine, './data/wine.csv')
df <- scale(wine[-1])
head(df)
wssplot(df)

# install.packages('NbClust')
library(NbClust)
set.seed(42)
nc <- NbClust(df, min.nc=2, max.nc=15, method='kmeans')
table(nc$Best.n[1,])

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]), 
        xlab='Number of Clusters', ylab='Number of Criteria', 
        main='Number of Clusers Chosen by 26 Criteria')

# we choose the best cluster number as 3

set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
fit.km$centers
head(df)
plot(df)
plot(df, col=fit.km$cluster)
points(fit.km$center, col=1:3, pch=8, cex=1.5)
# Alchole (x-axis), Malic (y-axis) plot...why??

# 각 군집별로 변수의 요약값을 측정단위의 척도로 나타냄
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)
fit.km$cluster

wine %>%
  mutate(cluster = fit.km$cluster) %>%
  select(-Type) %>%
  as_tibble() %>%
  group_by(cluster) %>%
  summarise_all(list(~mean(.)))

ct.tm <- table(wine$Type, fit.km$cluster)
ct.tm

# install.packages('flexclust')
library(flexclust)

# 실제 와인의 종류(Type)와 군집간의 일치도(agreement)를 나타내는 수정된 순위지수(adjusted rank index)를
# 구할 수 있음. 수정됨의 의미는 우연에 의해 발생되는 경우를 고려한 값이라는 의미임

randIndex(ct.tm)

data("Nclus")
plot(Nclus)

cl <- kcca(Nclus, k=4, family=kccaFamily('kmeans'))
image(cl)
points(Nclus)
barplot(cl)
# 각 군집의 변수별 중심이 전체 군집의 중심(상자 안의 막대)으로부터 얼마나 벗어나 있는지를 나타냄

stripes(cl)

# install.packages('cclust')
library(cclust)
cl.1 <- cclust(Nclus, 4, 20, method='kmeans')
plot(Nclus, col=cl.1$cluster)
points(cl.1$center, col=1:4, pch=8, cex=1.5)

library(cluster)
clusplot(Nclus, cl.1$cluster)

# 혼합 분포 군집
# mixture distribution clustering
# 흔히 혼합모형에서의 모수와 가중치의 추정에는 EM 알고리즘이 사용됨
# E-단계 :: 잠재변수 Z의 기대치 계산
# M-단계 :: 잠재변수 Z의 기대치를 이용하여 파라미터를 추정, 그리고 likelihood가 최대인지를 확인
# 로그-가능도함수가 최대가 되도록 반복

# install.packages('mixtools')
library(mixtools)
data("faithful")
attach(faithful)

hist(waiting, main='Time between Old Faithful erruptions', xlab='Minutes', ylab='', 
     cex.main=1.5, cex.lab=1.5, cex.axis=1.4)

wait1 <- normalmixEM(waiting, lambda=.5, mu=c(55, 80), sigma=5)
summary(wait1)

# summary of normalmixEM object:
#   comp 1   comp 2
# lambda  0.36085  0.63915
# mu     54.61364 80.09031
# sigma   5.86909  5.86909
# loglik at estimate:  -1034.00176 

plot(wait1, density=T, cex.axis=1.4, cex.main=1.8, 
     main2='Time between Old Faithful eruptions', xlab2='Minutes')

# install.packages('mclust')
library(mclust)
mc <- Mclust(iris[, 1:4], G=3)
summary(mc, parameters=T)

plot.Mclust(mc)
str(mc)
mc$classification

predict(mc, data=) # 'data='?? ?ǹ???

# 혼합분포군집은 k-평균군집의 절차와 유사하나 확률분포를 도입하여 군집을 수행하는 모형-기반 군집 방법
# 이상값 자ㄹ에 민감하므로 사전 조치 필요

# SOM

# 고차원의 데이터를 이해하기 쉬운 저차원의 뉴런으로 정렬하여 지도 형태로 형상화
# 이러한 형상화는 입력 변수의 위치관계를 그대로 보존
# 실제 공간의 입력변수가 가까이 있으면, 지도상에서도 가까운 위치에 있게 됨
# 입력층과 경쟁층
# 일력층 자료는 학습을 통하여 경쟁층에 정렬되는데, 이를 지도라 부름
# 입력층에 있는 각각의 뉴런은 경쟁층에 있는 각각의 뉴런들과 연결되어 있으며, 완전연결되어 있음
# 하나의 표본벡터(x)가 임의로 선택되었을 때, 프로토타입 벡터(경쟁층의 각각의 뉴런을 의미)와의 거리를
# 유클리드 거리에 의해 계산하고 비교
# 입력층 표본벡터에 가장 가까운 프로토타입 벡터를 BMU라고 함
# 승자 독점의 학습규칙에 따라 BMU 뿐만 아니라 위상학적 이웃에 대한 연결 강도를 조정함
# 이러한 경쟁학습으로 각각의 뉴런이 입력 벡터와 얼마나 가까운가를 계산하여 연결강도를 반복적으로 재조정하여 학습
# 연결강도는 입력 패턴과 가장 유사한 경쟁층 뉴런이 승자
# 승자독식 구조로 인해, 경쟁층에는 승자 뉴런만이 나타나며, 승자와 유사한 연결강도를 갖는 입력 패턴이 동일한 
# 경쟁 뉴런으로 배열
# 하나의 전방패스를 사용, 수행 속도 빠름, 실시간 학습 가능


# install.packages('kohonen')
packageurl <- "https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.19.tar.gz"
install.packages(packageurl, repos = NULL, type = "source")

library(kohonen)
library(RColorBrewer)

# install.packages('RCurl')
library(RCurl)
NBA <- read.csv(text = getURL("https://raw.githubusercontent.com/clarkdatalabs/soms/master/NBA_2016_player_stats_cleaned.csv"), 
                sep = ",", header = T, check.names = FALSE)

colnames(NBA)
NBA.measures1 <- c("FTA", "2PA", "3PA")
NBA.SOM1 <- som(scale(NBA[NBA.measures1]), grid = somgrid(6, 4, "rectangular"))
plot(NBA.SOM1)

colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}

plot(NBA.SOM1, type = "counts", palette.name = colors, heatkey = TRUE)

par(mfrow = c(1, 2))
plot(NBA.SOM1, type = "mapping", pchs = 20, main = "Mapping Type SOM")
plot(NBA.SOM1, main = "Default SOM Plot")
# error occur
# NBA.SOM2 <-som(scale(NBA[NBA.measures1]), 
#                grid=somgrid(6, 6, 'hexagonal'), 
#                toroidal=T)

# select random input
# compute winner neuron
# update neurons
# repeat for all input data
# classify input data

# classif :: 승자 유니트

# install.packages('arules')
library(arules)
data(Adult)
head(Adult)

rules <- apriori(Adult)
inspect(head(rules))
adult.rules <- apriori(Adult, parameter=list(support=.1, confidence=.6),
                       appearance = list(rhs=c('income=small', 'income=large'), default='lhs'),
                       control = list(verbose=F))
adult.rules.sorted <- sort(adult.rules, by='lift')
inspect(head(adult.rules.sorted))

# install.packages('arulesViz')
library(arulesViz)
plot(adult.rules.sorted, method='scatterplot')
plot(adult.rules.sorted, method='graph', control=list(type='items', alpha=.5))
