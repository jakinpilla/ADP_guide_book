data("ChickWeight")
head(ChickWeight)
dim(ChickWeight)
colnames(ChickWeight)

# install.packages('tidyverse')
library(tidyverse)

ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) + geom_line()

ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) + geom_point(alpha=.3) + geom_smooth(alpha=.2, size=1)

h <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet))
h + geom_point(alpha=.3)
h + geom_point(alpha=.3) + geom_smooth(alpha=.4, size=3)

ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) + geom_density()

ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) + 
  geom_histogram(colour='black', binwidth = 50) + facet_grid(Diet~.)

ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) + 
  geom_histogram(colour='black', binwidth = 50) + facet_grid(.~Diet)

data(mtcars)
head(mtcars)

p <- qplot(wt, mpg, colour=hp, data=mtcars)
p + coord_cartesian(ylim=c(0,40)) + scale_color_continuous(breaks = c(100, 300))
p + coord_cartesian(ylim=c(0,40)) + scale_color_continuous(breaks = c(100, 300)) + guides(colour='colourbar')

m <- mtcars[1:10, ]
p %+% m
str(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
str(mtcars)

c <- ggplot(mtcars, aes(factor(cyl)))
c + geom_bar()
c + geom_bar(fill='red')
c + geom_bar(fill='white', colour='red')
k <- ggplot(mtcars, aes(factor(cyl), fill=factor(vs)))
k + geom_bar()

head(ChickWeight)
ggplot(ChickWeight, aes(x=weight)) + geom_histogram(aes(fill=..count..)) 

data("economics")
head(economics)

b <- ggplot(economics, aes(x=date, y=unemploy))
b + geom_line()
b + geom_line(colour='red', size=3)
b + geom_line(linetype=2)

df <- data.frame(x=rnorm(5000), y=rnorm(5000))
h <- ggplot(df, aes(x, y))
h + geom_point()
h + geom_point(alpha=.5)
h + geom_point(alpha=1/10)

p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point(size=4)
p + geom_point(aes(colour=factor(cyl)), size=4)
p + geom_point(aes(shape=factor(cyl)), size=4)               

library(reshape2)
library(plyr)

rescale01 <- function(x) (x-min(x)) / diff(range(x))

rescale01 <- function(x) {
  (x-min(x)) / diff(range(x)) 
}

rescale01(economics$psavert)

range(economics$psavert)
range(rescale01(economics$psavert))
head(economics)
colwise(rescale01)(economics[, -c(1:2)])
colwise(rescale01)(economics[, -(1:2)])

ec_scaled <- data.frame(
  date=economics$date,
  colwise(rescale01)(economics[, -(1:2)])
)

head(ec_scaled)
str(ec_scaled)
ecm <- melt(ec_scaled, id='date')

unique(ecm$variable)
ecm$variable

f <- ggplot(ecm, aes(date, value))
f + geom_line(aes(linetype=variable, colour = variable), size=.8)

data("diamonds")
head(diamonds)
str(diamonds)

k <- ggplot(diamonds, aes(carat, ..density..)) + geom_histogram(binwidth = .2)
k + facet_grid(.~cut)


w <- ggplot(diamonds, aes(clarity, fill=cut))
w + geom_bar()
w + geom_bar(aes(order=desc(cut)))

str(mtcars)
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point(aes(size=qsec, colour = cyl))
p + geom_point(aes(size=qsec, colour = cyl)) + geom_hline(yintercept=25, size=2.5)

df2 <- data.frame(x=1:5, y=1:25, z=1:25)
df2
s <- ggplot(df2, aes(x=x, y=y))
s + geom_point()
s + geom_point(aes(shape=z), size=4) + scale_shape_identity()

# geom_pointrange() with lm's "se" option
dmod <- lm(price ~ cut, data=diamonds)
unique(diamonds$cut)
summary(dmod)
plot(dmod)
unique(diamonds$cut)
data.frame(cut=unique(diamonds$cut))
predict(dmod, data.frame(cut=unique(diamonds$cut)), se=T)
predict(dmod, data.frame(cut=unique(diamonds$cut)), se=T)[c('fit', 'se.fit')]

cuts <- data.frame(cut=unique(diamonds$cut), 
           predict(dmod, data.frame(cut=unique(diamonds$cut)), se=T)[c('fit', 'se.fit')])

se <- ggplot(cuts, aes(x=cut, y=fit, ymin=fit-se.fit, ymax=fit + se.fit, colour=cut))
se + geom_pointrange()

# annotate box
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
cuts
p + annotate('rect', xmin=2, xmax=3.5, ymin=2, ymax=25, fill='dark grey', alpha=.5)

# geom_smooth() and scale_x_continuous()
p <- qplot(disp, wt, data=mtcars) + geom_smooth()
p + scale_x_continuous(limits=c(325, 500))

# stat_bin2d() and scale_x_continuous()
d <- ggplot(diamonds, aes(carat, price))
d + geom_point()
d + stat_bin2d(bins = 25, colour='grey50') + scale_x_continuous(limits=c(0,2))

# boxplot
qplot(cut, price, data=diamonds, geom='boxplot')
last_plot() + coord_flip()

dim(diamonds)
qplot(cut, data=diamonds, geom='bar')
h <- qplot(carat, data=diamonds, geom='bar')
h
h + coord_flip()
h + coord_flip() + scale_x_reverse()

# multiple axis
time <- seq(7000, 3400, -200)
pop <- c(200, 400, 450, 500, 300, 100, 400, 700, 830, 1200, 400, 350,
         200, 700, 370, 800, 200, 100, 120)
length(pop)
grp <- c(2,5, 8, 3, 2, 2, 4, 7, 9, 4, 4, 2, 2, 7, 5, 12, 5, 4, 4)
length(grp)
med <- c(1.2, 1.3, 1.2, 0.9, 2.1, 1.4, 2.9, 3.4, 2.1, 1.1, 1.2, 1.5, 1.2, 0.9, 
         0.5, 3.3, 2.2, 1.1, 1.2)
length(med)
par(mar=c(5, 12, 4, 4) + .1)

plot(time, pop, axes=F, ylim=c(0,max(pop)), xlab='', ylab='', type='l', col='black', main='', xlim=c(7000, 3400))
points(time, pop, pch=20, col='black')
axis(2, ylim=c(0,max(pop)), col='black', lwd=2)
mtext(2, text='Population' , line=2)

par(new=T)
plot(time, med, axes=F, ylim=c(0, max(med)), xalb='', ylab='', 
     type='l', lty=2, main='', xlim=c(7000, 3400), lwd=2)
axis(2, ylim=c(0,max(med)), lwd=2, line=3.5)
points(time, med, pch=20)
mtext(2, text='Median Group Size', line=5.5)

par(new=T)
plot(time, grp, axes=F, ylim = c(0,max(grp)), xlab='', ylab='', type='l', 
     lty=3, main='', xlim=c(7000, 3400), lwd=2)
axis(2, ylim=c(0,max(grp)), lwd=2, line=7)
points(time, grp, pch=20)
mtext(2, text='Number of Groups', line=9)

axis(1, pretty(range(time), 10))
mtext('cal BP', side=1, col='black', line=2)

legend(x=7000, y=12, legend=c('Population', 'Median Group Size', 
                              'Number of Groups'), lty=c(1,2,3))


# Spatial Analysis
# install.packages('googleVis')
library(googleVis)

data(Fruits)
head(Fruits)

M1 <- gvisMotionChart(Fruits, idvar='Fruit', timevar='Year')
plot(M1) # not showing

data("Exports")
head(Exports)
G1 <-gvisGeoChart(Exports, locationvar = 'Country', colorvar = 'Profit')
plot(G1)
G2 <- gvisGeoChart(Exports,'Country', 'Profit', options=list(region='150'))
plot(G2)

require(datasets)
states <- data.frame(state.name, state.x77)
head(states)
G3 <- gvisGeoChart(states, 'state.name', 'Illiteracy', 
                   options = list(region='US', displayMode='regions', 
                                  resolution='metros', width=1000, height=400))
plot(G3)

data("CityPopularity")
head(CityPopularity)

G4 <- gvisGeoChart(CityPopularity, locationvar = 'City', colorvar = 'Popularity', 
                   options=list(region='US', height=350, displayMode='markers', 
                                colorAxis="{values:[200,400,600,800], colors:[\'red', \'pink', \'orange', \'green']}"))
plot(G4)

data(Andrew)
head(Andrew)
G5 <- gvisGeoChart(Andrew, "LatLong", colorvar='Speed_kt', options=list(region="US"))
plot(G5)

G6 <- gvisGeoChart(Andrew, "LatLong", sizevar='Speed_kt', colorvar = 'Pressure_mb', 
                   options=list(region="US"))
plot(G6)

# Create lat:long values and plot a map of Oceania
# Set background color to light-blue

require(stats)
data("quakes")
head(quakes)
quakes$latlong <- paste(quakes$lat, quakes$long, sep=':')
head(quakes$latlong)

G7 <- gvisGeoChart(quakes, 'latlong', colorvar = 'depth', sizevar = 'mag', 
                   options=list(displayMode='Markers', 
                                region='009', 
                                colorAxis="{colors:['red', 'grey']}",
                                backgroundColor='lightblue'))
plot(G7)

# install.packages("XML")
library(XML)
url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_credit_rating'
x <- readHTMLTable(readLines(url), which=3)
str(x)
colnames(x)
head(x)

as.character(unname(unlist(x[1, ]))) # 첫 번째 열의 문자열들을 하나의 벡터로 가져오기
colnames(x) <- as.character(unname(unlist(x[1, ])))
head(x)
x <- x[-1, ]
head(x)
rownames(x) <- NULL # re-indexing the dataframe
dim(x)

levels(x$Rating)
levels(x$Rating) <- substring(levels(x$Rating), 4, nchar(levels(x$Rating)))
x$Ranking <- x$Rating
x$Rating <- paste(x$`Country/Region`, x$Rating, sep=':')

head(x)
G8 <- gvisGeoChart(x, 'Country/Region', 'Ranking', hovervar='Rating', 
                   options=list(gvis.editor='S&P', 
                                colorAxis="{colors:['#91BFDB', '#FC8D59']}"))
plot(G8)

# https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php
# 상기 사이트에서 data download
library(XML)
eq <- read.csv('./data/2.5_week.csv')
head(eq)
eq$loc <- paste(eq$latitude, eq$longitude, sep=':')

G9 <- gvisGeoChart(eq, 'loc', 'depth', 'mag', 
                   options=list(displayMode="Markers", 
                                colorAxis="{colors:['purple', 'red', 'orange', 'grey']}", 
                                backgroundColor="lightblue"), chartid = "EQ")
plot(G9)


