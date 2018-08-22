getwd()
setwd("C:/Users/dsc/adp_guidebook")

# install.packages('RODBC')
library(RODBC)
new <- odbcConnectExcel('./data/ie_data.xls')

# odbcConnectExcel is only usable with 32-bit Windows
new

mydata <- sqlFetch(new, 'Data')
head(mydata, 10)

rep(1, 3)
seq(1, 3)
1:3
seq(1, 11, 2)
seq(1, 11, length=6)
seq(1, 11, length=8)
rep(2:5, 3)
a = 1:10
a+a
a*a
a/a
a=c(2, 7, 3)
a
t(a)
a%*%t(a)
mx = matrix(c(23, 41, 12, 35, 67, 1, 24, 7, 53), nrow=3)
mx
5*mx
solve(mx)
a=1:10
a
mean(a)
sd(a)
sum(a)
median(a)
log(a)
b = log(a)
cov(a, b)
cor(a, b)
summary(a)
b = c('a', 'b', 'c', 'd', 'e')
b[2]
b[-4]
b[c(2, 3)]
income=c(100,200,150,300,900)
car=c('kia', 'hyundai', 'kia', 'toyota', 'lexus')
marriage=c(F, F, F, T, F)
mydat = data.frame(income, car, marriage)
mydat
mydat[3, 2]
mydat[,2]
mydat[4,]

a=c()
for (i in  1:9) {
  a[i] = i*i
}
a

isum=0
for (i in 1:100) {
  isum = isum + i
}
cat('1부터 100까지의 합=', isum, '입니다.', '\n')

x = 1
while( x < 5) {
  x=x+1
  print(x)
}

StatScore = c(88, 90, 78, 84, 76, 68, 50, 48, 33, 70, 48, 66, 88, 96, 79, 65, 27, 88, 96, 
              33, 64, 48, 77, 18, 26, 44, 48, 68, 77, 64, 88, 95, 79, 88, 49, 30, 29, 10, 49, 88)
length(StatScore)
over70 = rep(0,40)

for (i in 1:40) { if (StatScore[i] >= 70) over70[i] = 1 
else over70[i] = 0
}
over70
sum(over70)

addto = function(a) {
  isum = 0
  for(i in 1:a) {
    isum = isum+i
  }
  print(isum)
}

addto(100) 
addto(50)

number=1:10
alphabet=c('a', 'b', 'c')
paste(number, alphabet)
paste(number, alphabet, sep=' to the ')
substr('BigDataAnalysis', 1, 4)
country = c('Korea', 'Japan', 'China', 'Singapore', 'Russia')
substr(country, 1, 3)

as.integer(3.14)
as.numeric('foo')
as.logical(0.45)
as.logical(0)

mydat
as.matrix(mydat)
as.Date('2015-01-13')
as.Date('01/13/2015', format='%m/%d/%Y')
as.Date('08/13/2013', format='%m/%d/%Y')
format(Sys.Date())
as.character(Sys.Date())
format(Sys.Date(), format='%m/%d/%Y')
format(Sys.Date(), '%a')
format(Sys.Date(), '%b')
format(Sys.Date(), '%m')
format(Sys.Date(), '%d')
format(Sys.Date(), '%y')
format(Sys.Date(), '%Y')

format(as.Date("08/13/2013", format="%m/%d/%Y"), '%a')

height = c(170, 168, 174, 175, 188, 165, 165, 190, 173, 168, 159, 170, 184, 155, 165)
weight = c(68, 65, 74, 77, 92, 63, 67, 95, 72, 69, 60, 69, 73, 56, 55)
plot(height, weight)
pairs(iris[1:4], main = "Andeeson's Iris Dtat -- 3 species", 
      pch=21, bg=c('red', 'green3', 'blue')[unclass(iris$Species)])

StatScore
hist(StatScore, prob=T)
boxplot(StatScore)

#reshape :: melt, cast...
# install.packages('reshape')
library(reshape)
data("airquality")
head(airquality)
names(airquality)
names(airquality) = tolower(names(airquality))
names(airquality)
aqm = melt(airquality, id=c('month', 'day'), na.rm=T)
aqm

a <- cast(aqm, day~month~variable)
a
b <- cast(aqm, month ~ variable, mean)
b
c <- cast(aqm, month ~. |variable, mean)
c
d <- cast(aqm, month ~ variable, mean, margins=c('grand_row', 'grand_col'))
d
e <- cast(aqm, day ~ month, mean, subset=variable=='ozone')
e
f <- cast(aqm, month ~ variable, range)
f
# sqldf
# install.packages('sqldf')
library(sqldf)
sqldf('select * from iris')
sqldf('select * from iris limit 10')
sqldf("select count(*) from iris where Species like 'se%'")

# ddply
library(tidyverse)

runif(9,0,20)

set.seed(1)
d = data.frame(year=rep(2012:2014, each=6), count=round(runif(9, 0, 20)))
d
ddply(d, 'year', function(x) {
  mean.count = mean(x$count)
  sd.count = sd(x$count)
  cv = sd.count/mean.count
  data.frame(cv.count=cv)
})

head(d)
str(d)
as.factor(d$year) -> d$year
d
d %>% group_by(year) %>% summarise(mean.count=mean(count))
d %>% group_by(year) %>% summarise(sum.count=sum(count))

iris %>% group_by(Species) %>% summarise(mean.Sepal.Length=mean(Sepal.Length))
mtcars %>% group_by(cyl, gear) %>% summarise(newvar = sum(wt))

ddply(d, 'year', summarise, mean.count=mean(count))
d %>% group_by(year) %>%  summarise(mean.count = mean(count))

ddply(d, 'year', transform, total.count = sum(count))
d %>% group_by(year) %>% transform(total.count = sum(count))

# data.table
# install.packages('data.table')
library(data.table)
DT = data.table(x=c('b', 'b', 'b', 'a', 'a'), v=rnorm(5))
DT
data(cars)
head(cars)
CARS <- data.table(cars)
head(CARS)
tables()
sapply(CARS, class)
DT
DT[2,]
DT[DT$x=='b', ]
DT[x=="b", ]

runif(10)
grpsize <- ceiling(1e7/26^2)
grpsize
grpsize*26
grpsize*26*26
tt <- system.time(DF <- data.frame(
  x=rep(LETTERS, each=26*grpsize),
  y=rep(letters, each=grpsize),
  v=runif(grpsize*26^2),
  stringsAsFactors = F
))
tt

head(DF, 3)
tail(DF, 3)
dim(DF)
tt <- system.time(ans1 <- DF[DF$x == 'R' & DF$y == 'h', ])
tt
head(ans1)
head(DF,100)
DT <- data.table(DF)
setkey(DT,x,y)
ss <- system.time(ans2 <- DT[J('R', 'h')]) # binary search
ss

# bad case for using data.table, # 1.425 secs
system.time(ans2 <- DF[DF$x == 'R' & DF$y == 'h', ])
mapply(identical, ans1, ans2)
DT[, sum(v)]
DT[, sum(v), by=x]

ttt <- system.time(tt <- tapply(DT$v, DT$x, sum))
ttt
sss <- system.time(ss <- DT[, sum(v), by=x])
sss <- system.time(ss <- DT[, sum(v), by='x,y'])
sss

data(iris)
head(iris)
str(iris)
summary(iris)
cov(iris[, 1:4])
cor(iris[, 1:4])

# NA
y <- c(1,2,3,NA)
is.na(y)
head(mydat)
# mydata[mydata$v1==99, 'v1'] <- NA

x <- c(1,2,NA,3)
mean(x)
mean(x, na.rm=T)

# install.packages('Amelia')
library(Amelia)
data("freetrade")
head(freetrade)
dim(freetrade)
str(freetrade)

a.out <- amelia(freetrade, m=5, ts='year', cs='country')
hist(a.out$imputations[[3]]$tariff, col='gray', border='white')
save(a.out, file = 'imputation.RData')
write.amelia(obj=a.out, file.stem = 'outdata')
missmap(a.out)

freetrade$tariff <- a.out$imputations[[5]]$tariff
missmap(freetrade)

# ouliers
x = rnorm(100)
boxplot(x)
x <- c(x, 19, 28, 30)
outwidth = boxplot(x)
outwidth$out

# install.packages('outliers')
library(outliers)
set.seed(1234)
y = rnorm(100)
outlier(y)
outlier(y, opposite = T)

a=c(10,20,30)
b=c(40,30,20)
a
t(b)
x=a%*%t(b)
x
y=a*b
y

# static analysis

head(iris)
library(MASS)
data(Animals)
head(Animals)

# regression
set.seed(2)
x = runif(10,0,11)
y = 2 + 3*x + rnorm(10,0,.2)
dfrm = data.frame(x,y)
dfrm
lm(y~x, data=dfrm)

set.seed(2)
u = runif(10,0,11)
v = runif(10,11,20)
w = runif(10,1,30)
y = 3 + .1*u + 2*v -3*w + rnorm(10,0,.1)
dfrm = data.frame(y, u, v, w)
dfrm

m <- lm(y~u+v+w)
m
summary(m)

library(MASS)
head(ChickWeight)
summary(ChickWeight)
dim(ChickWeight)
ChickWeight$Chick

ChickWeight %>% filter(Chick == 5)
length(ChickWeight$Chick)
head(ChickWeight)
str(ChickWeight)


ChickWeight %>% filter((Chick == 1) & (Diet == 1)) -> Chick
lm(weight ~ Time, Chick) -> lm_chick
summary(lm_chick)

head(cars)
cars$speed2 <- cars$speed^2
head(cars)
cars[, c(3,1,2)] %>% head()
lm(dist ~ speed + speed2, data=cars)
summary(lm(dist ~ speed + speed2, data=cars))

x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
y <- c(5, 3, 2, 3, 4, 6, 10, 12 ,18)
df1 <-data.frame(x, y)
df1
plot(df1)
library(ggplot2)
ggplot(data=df1) + geom_point(aes(df1$x, df1$y))
ggplot(data=df1, aes(x, y)) + geom_point(color='blue', alpha=.5, size=3) 

df1$x2 <- x^2
df1
lm(y~x, data=df1)
summary(lm(y~x, data=df1))
plot(lm(y~x, data=df1))

lm(y~x+x2, data=df1)
summary(lm(y~x+x2, data=df1))
plot(lm(y~x+x2, data=df1))

x1 <- c(7, 1,11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10)
x2 <- c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68)
x3 <- c(6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8)
x4 <- c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12)

Y <- c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1, 115.9, 83.9, 113.3, 109.4)
df <- data.frame(x1, x2, x3, x4, Y)
head(df)
write.csv(df, './data/df_p390.csv')

a <- lm(Y ~ x1 + x2 + x3 + x4, data = df)
a
summary(a)

b <- lm(Y ~ x1 + x2 + x4, data=df)
b
summary(b)
c <- lm(Y ~x1+x2, data=df)
c
summary(c)

step(lm(Y ~ 1, df), scope=list(lower=~1, upper=~x1+x2+x3+x4), direction='forward')
step(lm(Y ~ 1, df), scope=list(lower=~1, upper=~x1+x2+x3+x4), direction='both')

data(hills)
head(hills)
step(lm(time ~ 1, hills), scope=list(lower=~1, upper=~dist+climb), direction='forward')

age <- c(7, 7, 8, 8, 8, 9, 11, 12, 12, 13, 13, 14, 14, 15, 16, 17, 17, 17, 17, 19, 19, 20, 23, 23, 23)
length(age)
height <- c(109, 112, 124, 125, 127, 130, 139, 150, 146, 155, 156, 153, 160, 158, 160, 153, 174, 176, 171, 156, 
            174, 178, 180, 175, 179)
length(height)
weight <- c(13.1, 12.9, 14.1, 16.2, 21.5, 17.5, 30.7, 28.4, 25.1, 31.5, 39.9, 42.1, 45.6, 51.2, 35.9, 
            34.8, 44.7, 60.1, 42.6, 37.2, 54.6, 64, 73.8, 51.1, 71.5)
length(weight)
bmp <- c(68, 65, 64, 67, 93, 68, 89, 69, 67, 68, 89, 90, 93, 93, 66, 70, 70, 92, 69, 72, 86, 86, 97, 71, 95)
length(bmp)
fev <- c(32, 19, 22, 41, 52, 44, 28, 18, 24, 23, 39, 26, 45, 45, 31, 29, 49, 29, 38, 21, 37, 34, 57, 33, 52)
length(fev)
rv <- c(258, 449, 441, 234, 202, 308, 305, 369, 312, 413, 206, 253, 174, 158, 302, 204, 187, 188, 172, 216,
        184, 225, 171, 224, 225)
length(rv)
frc <- c(183, 245, 268, 146, 131, 155, 179, 198, 194, 225, 142, 191, 139, 124, 133, 118, 104, 129, 130, 119, 118, 148, 108, 131, 127)
length(frc)
tlc <- c(137, 134, 147, 124, 104, 118, 119, 103, 128, 136, 95, 121, 108, 90, 101, 120, 103, 130, 103,
         81, 101, 135, 98, 113, 101)
length(tlc)
pemax <- c(95, 85, 100, 85, 95, 80, 65, 110, 70, 95, 110, 90, 100, 80, 134, 134, 165, 120, 130, 85, 85, 160, 165, 95, 195)
length(pemax)

bio <- data.frame(age, weight, bmp, fev, rv, frc, tlc, pemax)
write.csv(bio, './data/bio.csv', row.names = F)
bio <- read.csv('./data/bio.csv')
head(bio)
step(lm(pemax~1, bio), scope=list(lower=~1, upper=~age+height+weight+bmp+rv+frc+tlc), direction = 'forward')
step(lm(pemax~age+height+weight+bmp+rv+frc+tlc, bio), direction = 'backward')
step(lm(pemax~1, bio), scope=list(lower=~1, upper=~age+height+weight+bmp+rv+frc+tlc), direction = 'both')

# install.packages('Hmisc')
library(Hmisc)
data(mtcars)
head(mtcars)
dim(mtcars)
drat <- mtcars$drat
disp <- mtcars$disp
plot(drat, disp)
cor(drat, disp)
rcorr(as.matrix(mtcars), type='pearson')
cov(mtcars)

rcorr(as.matrix(mtcars), type='spearman')

korean <- c(85, 75, 65, 78, 59, 60, 90, 100, 99, 91, 70)
math <- c(80, 60, 75, 40, 50, 64, 70, 78, 90, 98, 50)
english <- c(80, 70, 69, 79, 80, 95, 98, 97, 67, 80, 59)
science <- c(90, 100, 50, 80, 67, 89, 60, 79, 89, 80, 100)
test <- data.frame(korean, math, english, science)
test
rcorr(as.matrix(test), type='spearman')

data(eurodist)
eurodist      
loc <- cmdscale(eurodist)
x <- loc[,1]
y <- loc[,2]
plot(x, y, type='n', main='eurodist')
text(x, y, rownames(loc), cex=.8)
abline(v=0, h=0)

library(datasets)
data(USArrests)
head(USArrests)
summary(USArrests)

fit <- princomp(USArrests, cor=T)
summary(fit)
loadings(fit)
fit$scores
biplot(fit)
par(mfrow=c(1, 1))
nm <- c('쇠고기라면', '해물라면', '얼큰라면', '떡라면', '짬뽕라면', '만두라면', '치즈라면', '된장라면', 
        '볶음라면', '김치라면')
nodle <- c(2, 1, 5, 2, 3, 4, 4, 1, 3, 5)
bowl <- c(4, 5, 3, 2, 5, 3, 4, 2, 3, 5)
soup <- c(5, 1, 4, 3, 5, 2, 3, 1, 2, 3)
nodle_data <- data.frame(nodle, bowl, soup, row.names = nm)
nodle_data
write.csv(nodle_data, './data/nodle_data.csv', fileEncoding = 'utf-8')
rownames(nodle_data)

library(readr)
nodle_data <- read_csv('./data/nodle_data.csv')
nodle_data
nodle_data <- data.frame(column_to_rownames(nodle_data, var= "X1"))
nodle_data
p1 = prcomp(nodle_data, scale=T)
p1
summary(p1)
predict(p1)
biplot(p1)

Price <- c(6,7,6,5,7,6,5,6,3,1,2,5,2,3,1,2)
Software <- c(5,3,4,7,7,4,7,5,5,3,6,7,4,5,6,3)
Aesthetics <- c(3,2,4,1,5,2,2,4,6,7,6,7,5,6,5,7)
Brand <- c(4,2,5,3,5,3,1,4,7,5,7,6,6,5,5,7)
data <- data.frame(Price, Software, Aesthetics, Brand)
write.csv(data, './data/computer_data_p419.csv', row.names = F)
data <- read.csv('./data/computer_data_p419.csv')
head(data)

pca <- princomp(data, cor=T)
summary(pca)
predict(pca)
biplot(pca)
head(data)

# ARIMA
Nile
ldeaths
str(Nile)
str(ldeaths)

plot(Nile)
plot(ldeaths)

ldeaths.decompose <- decompose(ldeaths)
ldeaths.decompose$seasonal
plot(ldeaths.decompose)
ldeaths.decompose.adj <- ldeaths - ldeaths.decompose$seasonal
plot(ldeaths.decompose.adj)

Nile.diff1 <- diff(Nile, differences=1)
plot(Nile.diff1)

Nile.diff2 <- diff(Nile, differences=2)
plot(Nile.diff2)

# 자기상관함수(acf), 부분자기상관함수

acf(Nile.diff2, lag.max=20)
acf(Nile.diff2, lag.max=20, plot=F)

pacf(Nile.diff2, lag.max=20)
pacf(Nile.diff2, lag.max=20, plot=F)

# install.packages('forecast')
library(forecast)
auto.arima(Nile)
Nile.arima <-  arima(Nile, order=c(1,1,1))
Nile.arima
Nile.forecasts <- forecast(Nile.arima, h=10)
plot(Nile.forecasts)

# exercise firm.csv data
firm <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
length(firm)
salary <- c(3030, 6050, 3571, 3300, 0, 9375, 9525, 5000, 999, 3300, 3500, 2493, 
            1911, 2130, 1185, 5236, 1990, 6000, 6229, 1523)
length(salary)
tenure <- c(7, 0, 11, 6, 18, 6, 15, 5, 3, 2, 16, 5, 7, 4, 0, 2, 4, 32, 5, 3)
length(tenure)
age <- c(61, 51, 63, 60, 63, 57, 60, 61, 57, 60, 63, 61, 58, 59, 56, 60, 60, 74, 63, 56)
length(age)
sales <- c(161315, 144416, 139208, 100697, 100469, 81667, 76431, 57813, 56154, 53588,
           50777, 47678, 47061, 41322, 37154, 35853, 33674, 33296, 32379, 31707)
length(sales)
profits <- c(2956, 22071, 4430, 6370, 9296, 6328, 5807, 5372, 1120, 6398, 5165, 1704, 2945, 1048, 3780, 1259, 
             568, 3765, 3782, 578)
length(profits)
assets <- c(257389, 237545, 49271, 92630, 355935, 86100, 668641, 59920, 36672, 59550, 617679,
            42754, 33673, 37675, 30966, 299804, 14166, 19166, 194398, 3665875)
length(assets)

firm <- data.frame(firm, salary, tenure, age, sales, profits, assets)
write.csv(firm, './data/firm.csv', row.names = F)
read.csv('./data/firm.csv')
summary(firm)
var(firm$salary)
sd(firm$salary)
median(firm$salary)

for (i in 2:7) {
  mean_value = mean(firm[, i])
  sd_value = sd(firm[, i])
  median_value = median(firm[, i])
  print(mean_value)
  print(sd_value)
  print(median_value)
  }

library(ggplot2)
ggplot(data = firm, aes(x = profits, y = salary)) + geom_point(color='blue')
ggplot(firm, aes(profits, salary)) + geom_point(color='blue', size=3)

firm_lm_1 <- lm(salary ~ profits, data = firm)
summary(firm_lm_1)

firm_lm_2 <- lm(salary ~ profits + age + sales, data=firm)
summary(firm_lm_2)

firm_lm_all <- lm(salary  ~ ., data= firm)
summary(firm_lm_all)

step(lm(salary ~ ., data=firm), direction='backward')
step(lm(salary ~ 1., data=firm), scope = list(lower=~1, upper=~tenure+sales+profits+assets),direction='forward')
step(lm(salary ~ 1., data=firm), scope = list(lower=~1, upper=~tenure+sales+profits+assets),direction='both')

data(iris)
a <- subset(iris, Species == 'setosa' | Species == 'versicolor')
a$Species <- as.factor(a$Species)
str(a)
b <- glm(Species ~ Sepal.Length, family = binomial, data=a)
summary(b)
# p_value 거의 0 이므로 Sepal.Length는 매유 유의미한 변수

pchisq(138.629, df=99, lower.tail = F)
# [1] 0.005302078
pchisq(64.211, df=98, lower.tail = F)
# [1] 0.9966935
# 통계적으로 유의함으로 적합결여를 의미
# Null deviance에 비해 자유도 1 기준에 이탈도의 감소가 74.4 정도의 큰 감소를 보이며
# pchisq(64.211, df=98, lower.tail = F) = 0.9966935 이므로 귀무가설리 기각되지 않으며
# 적합된 관측값이 관측된 자료를 잘 적합하고 있다고 할 수 있음

coef(b)
exp(coef(b)['Sepal.Length'])
# Sepal.Length 
# 170.7732 
# Sepal.Length가 한 단위 증가함에 따라 Versicolor 오즈가 약 170배 증가함을 의미

confint(b, param='Sepal.Length')
# Sepal.Length   3.421613   7.41550
# 회귀계수의 신뢰구간

exp(confint(b, param='Sepal.Length'))
# 오즈의 증가량에 대한 신뢰구간

fitted(b)[c(1:5, 96:100)]
predict(b, newdata=a[c(1, 50, 51, 100), ], type='response')
cdplot(Species~Sepal.Length, data=a)
plot(a$Sepal.Length, a$Species, xlab = 'Sepal.Length')
x = seq(min(a$Sepal.Length), max(a$Sepal.Length), .1)
lines(x, 1+(1/(1+exp(-27.831 + 5.140*x))), type='l', col='red')

attach(mtcars)
str(mtcars)
glm_vs <- glm(vs ~ mpg + am, data=mtcars, family=binomial)
summary(glm_vs)

coef(glm_vs)
# 다른 모든 변수들(여기서는 am)이 주어질때 mpg값이 한 단위 증가함에 따라 vs가
# 1일 오즈가 exp(0.6809) ~ 1.98배 증가
# mpg가 주어질때 오즈에 대한 am의 효과는 exp(-0.0073) ~ 0.05 배 즉, 
# 변속기가 수동인 경우 자동에 비해 vs=1의 오즈가 95% 감소

pchisq(43.860, df=31, lower.tail = F)
pchisq(20.646, df=29, lower.tail = F)

step_vs <- step(glm_vs, direction='backward')
step_vs
summary(step_vs)
ls(glm_vs)
str(glm_vs)

anova(glm_vs, test='Chisq')
# 모형의 적합(변수가 추가되는) 단계별로 이탈도의 감소량과 유의성 검정 결과를 제시
#      Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                    31     43.860              
# mpg   1   18.327        30     25.533 1.861e-05 ***
# am    1    4.887        29     20.646   0.02706 * 
# p_value 모두 0.05 보다 작음
# 그 결과, 두 변수가 차례로 추가되면서 생겨나는 이탈도의 감소량이 모두 통계적으로 유의


# nnet

library(nnet)
nn_iris <- nnet(Species ~., data=iris, size=2, rang=.1, decay=5e-4, maxit=200)
summary(nn_iris)

# install.packages('clusterGeneration')
# install.packages('scales')
# install.packages('reshape')
library(clusterGeneration)
library(scales)
library(reshape)

table(iris$Species, predict(nn_iris, iris, type='class'))

# install.packages('neuralnet')

library(neuralnet)

data("infert")
head(infert)
str(infert)
summary(infert)
boxplot(infert)

library(tidyverse)
colnames(infert)
infert %>% select(-c('education', 'stratum', 'pooled.stratum'))-> infert_cont
infert_cont$case

net.infert <- neuralnet(case ~ age + parity + induced + spontaneous, data=infert, hidden=2, 
          err.fct='ce', linear.output = F, likelihood = T)

net.infert
plot(net.infert)

names(net.infert)
net.infert$result.matrix
net.infert$covariate
net.infert$net.result[[1]]
net.infert$generalized.weights
out <- cbind(net.infert$covariate, net.infert$net.result[[1]])
dimnames(out) <- list(NULL, c('age', 'party', 'induced', 'spontaneous', 'nn-output'))
head(out)
# generalized.weights가 제시하는 일반화 가중치는 각 공변량들의 효과를 나타내는 것
# 로지스틱 회귀모형에서의 회귀계수와 유사하게 해석
# 로지스틱회귀와는 달리 일반화가중치는 다른 모든 공변량에 의존하므로 각 자료점에서
# 국소적인 기여도를 나타냄
# 예를들어, 동일한 변수가 몇몇 관측치에 대해서는 양의 영향을 가지며, 또 다른 관측치에 대해서는
# 양의 영향을 가지며, 또 다른 관측치에 대해서는 음의 영향을 가지며, 평균적으로는 0에 가까운 영향을
# 갖는 것이 가능하다. 
# 모든 자료에 대한 일반화가중치의 분포는 특정 공변향의 효과가 선형적인지의 여부를 나타냄
# 즉, 작은 분산은 선형효과를 제시하며, 큰 분산은 관측치 공간상에서 변화가 심하다는 것을 
# 나타내므로 비-선형적인 효과가 있음을 나타냄
head(net.infert$generalized.weights[[1]])

# 일반화가중치에 대한 시각화
par(mfrow=c(2,2))
gwplot(net.infert, selected.covariate = 'age', min=-2.5, max=5)
gwplot(net.infert, selected.covariate = 'parity', min=-2.5, max=5)
gwplot(net.infert, selected.covariate = 'induced', min=-2.5, max=5)
gwplot(net.infert, selected.covariate = 'spontaneous', min=-2.5, max=5)
par(mfrow=c(1,1))
# 공변령 age는 모든 값이 0 근처의 값을 가지므로 사례-대조 상태에 따른 효과가 없으며,
# 적어도 2개의 공변량 induced와 spontaneous는 일반화 가중치의 분산이 전반적으로 1보다 크기 때문에
# 비선형 효과를 가짐. 모형의 단순화를 위해 age와 관련된 뉴런을 제외한 즉, 3개의 입력변수
# parity, induced, spontaneous 만으로 신경망모형을 적합할 수 있음.

covariate_mat = matrix(c(22, 1, 0, 0, 22, 1, 1, 0, 22, 1, 0, 1, 22, 1, 1, 1),
                 byrow=TRUE, ncol=4)
covariate_mat

new.output <- neuralnet :: compute(net.infert, covariate_mat) 
new.output$net.result

# [1,] 0.1498340673
# [2,] 0.1960744827
# [3,] 0.3095099949
# [4,] 0.8531143148(spontaneous)
# 사전 낙태의 수에 따라 예측 확률이 증가함을 의미

# 시그모이드(sigmoid) 활성함수를 가지는 2개 층의 네트워크(1개 은닉층)는 임의의 의사결정 경계를
# 모형화 할 수 있음

# 0과 100 사이의 난수를 50개 발생시키고, 제곱근을 취한 값을 결과로 하는 자료를 구축 후 자료를 신경망으로
# 학습하여 새로운 예측을 수행
train.input <- as.data.frame(runif(50, min=0, max=100))
train.output <- sqrt(train.input)
train.data <- cbind(train.input, train.output)
colnames(train.data) <- c('input','output')
head(train.data)

net.sqrt <- neuralnet(output ~ input, train.data, hidden=10, threshold=.01)
# threshold = 옵션은 오차함수의 편미분에 대한 값으로 정지규칙으로 사용됨

print(net.sqrt)
plot(net.sqrt)

test.data <- as.data.frame((1:10)^2)
test.out <- neuralnet :: compute(net.sqrt, test.data)
ls(test.out)
print(test.out$net.result)

net2.sqrt <- neuralnet(output ~ input, train.data, hidden=c(10, 8), threshold = .01)
plot(net2.sqrt)

test2.out <- neuralnet :: compute(net2.sqrt, test.data)
print(test2.out$net.result)

# 신경망 모형의 장점 :: 입,출력 변수간에 복잡한 비선형관계가 존재할 때 유용하며, 잡음에 대해서도
# 민감하게 반응하지 않음













































