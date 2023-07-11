### Forecasting Electricity price ###

library(ggplot2)
library(GGally)
library(tidyverse)
library(patchwork)
library(lubridate, warn.conflicts = FALSE)
library(dplyr)

data <- read.table('ml_data.csv',header = T, sep=",")
summary(data)
head(data)

data.18.19 <- data[1:26304,]
summary(data.18.19)

data.18.19 <- data[,-1]

#PCA 
vector = c(1,2,3,4,5,6,7,8,9, 20, 21, 22, 23, 24, 25) 
data.18.19 <- data.18.19[, -vector]
pc<-princomp(data.18.19,scores=T)
pc
summary(pc)

x11()
par( mfrow=c(3,1))
for (i in 1:3)
  barplot(pc$loadings[,i], ylim = c(-1,1), main=paste('Loadings PC'), i)
graphics.off()

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc, las=2, main='Principal components', ylim=c(0,4.5e7))
barplot(sapply(data.18.19,sd)^2, las=2, main='Original Variables', ylim=c(0,4.5e7), ylab='Variances')
plot(cumsum(pc$sd^2)/sum(pc$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data.18.19),labels=1:ncol(data.18.19),las=2)



