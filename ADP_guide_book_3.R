setwd("C:/Users/dsc/adp_guidebook")
setwd("C:/Users/jooyon/Desktop/ADP_guide_book-master/ADP_guide_book-master")
data("USArrests")
str(USArrests)

d <- dist(USArrests, method='euclidean')
fit <- hclust(d, method='ave')

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
rect.hclust(hca, h=50, which=c(2, 7), border=3:4)

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
wine <- read.csv(url, header=FALSE)
wine <- read.csv("./data/wine.csv", header=T)
head(wine)
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash',
                    'Alcalinity', 'Magnesium', 'Phenols',
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue',
                    'Dilution', 'Proline')

wine$Type <- as.factor(wine$Type)
head(wine)
write.csv(wine, './data/wine.csv')
head(wine[, -c(1,2)])
df <- scale(wine[, -c(1,2)])
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
fit.km <- kmeans(df,3, nstart=25)
fit.km$size

fit.km$centers
plot(df, col=fit.km$cluster)
points(fit.km$center, col=1:3, pch=8, cex=1.5)

# Alchole (x-axis), Malic (y-axis) plot...why??

aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

ct.tm <- table(wine$Type, fit.km$cluster)
ct.tm

# install.packages('flexclust')
library(flexclust)
randIndex(ct.tm)

data("Nclus")
plot(Nclus)

cl <- kcca(Nclus, k=4, family=kccaFamily('kmeans'))
image(cl)
points(Nclus)
barplot(cl)
stripes(cl)

# install.packages('cclust')
library(cclust)
cl.1 <- cclust(Nclus, 4, 20, method='kmeans')
plot(Nclus, col=cl.1$cluster)
points(cl.1$center, col=1:4, pch=8, cex=1.5)

library(cluster)
clusplot(Nclus, cl.1$cluster)

# mixture distribution clustering
# install.packages('mixtools')
library(mixtools)
data("faithful")
attach(faithful)

hist(waiting, main='Time between Old Faithful erruptions', xlab='Minutes', ylab='', 
     cex.main=1.5, cex.lab=1.5, cex.axis=1.4)

wait1 <- normalmixEM(waiting, lambda=.5, mu=c(55, 80), sigma=5)
summary(wait1)

plot(wait1, density=T, cex.axis=1.4, cex.main=1.8, 
     main2='Time between Old Faithful eruptions', xlab2='Minutes')

# install.packages('mclust')
library(mclust)
mc <- Mclust(iris[, 1:4], G=3)
summary(mc, parameters=T)

plot.Mclust(mc)
str(mc)
mc$classification
predict(mc, data=) # 'data='ÀÇ ÀÇ¹Ì??

# SOM
# install.packages('kohonen')
packageurl <- "https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.19.tar.gz"
install.packages(packageurl, repos = NULL, type = "source")

require(kohonen)
require(RColorBrewer)

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
