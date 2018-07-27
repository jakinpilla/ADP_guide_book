getwd()
setwd("C:/Users/dsc/adp_guidebook")

# install.packages('RODBC')
library(RODBC)
new <- odbcConnectExcel('./data/ie_data.xls')
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
library(plyr)

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

ddply(d, 'year', summarise, mean.count=mean(count))
ddply(d, 'year', transform, total.count = sum(count))

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
ChickWeight %>% group_by(Chick) %>% summarise(sum.Chick = count(ChickWeight$Chick)) -> chicksum
chicksum
chicksum$sum.Chick -> chicksum
chicksum %>% filter(chicksum$freq != 12) ## 12개의 데이터가 없는 닭 :: 18(2) / 16(7) / 15(8) / 8(11) / 44(10)
colnames(ChickWeight)
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











