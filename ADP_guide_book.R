getwd()
setwd("C:/Users/dsc/adp_guide")

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
z=c




















