setwd("C:/Users/dsc/adp_guidebook")

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
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash',
                    'Alcalinity', 'Magnesium', 'Phenols',
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue',
                    'Dilution', 'Proline')

wine$Type <- as.factor(wine$Type)
head(wine)
write.csv(wine, './data/wine.csv')

df <- scale(wine[-1])
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

set.seed(1234)
fit.km <- kmeans(df,3, nstart=25)
fit.km$size

fit.km$centers
plot(df, col=fit.km$cluster)
points(fit.km$center, col=1:3, pch=8, cex=1.5)



