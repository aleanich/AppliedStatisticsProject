### Forecasting Electricity price ####

library(ggplot2)
library(GGally)
library(tidyverse)
library(patchwork)
library(lubridate, warn.conflicts = FALSE)
library(dplyr)

data <- read.table('ml_data.csv',header = T, sep=",")
summary(data)
head(data)

dates <- ymd_hms(data$X)

# Visualizing dam electricity price

data = data.frame(dates, data[,2:30])  # dates with the right format for plotting time series

x11()
p <- ggplot(data,aes(x=dates,y=dam)) ### what's happened???????
p + geom_line()

# Visualizing other variables

#Swiss_import

x11()
q <- ggplot(data,aes(x=dates,y=Swiss_import))
p + q + geom_line()

x11()
h <- ggplot(data, aes(x=dates)) +  geom_line( aes(y=dam), color="blue") + geom_line( aes(y=gas_price),size = 1, color="green") + scale_y_continuous (name = "dam",sec.axis = sec_axis(~.*1, name="Gas Price"))
h

# gas price

x11()
p <- ggplot(data,aes(x=dates,y=gas_price)) ### as dam price
p + geom_line()

# wind10

x11()
p <- ggplot(data,aes(x=dates,y=wind10))
p + geom_line()

#Forecasted_load

x11()
p <- ggplot(data,aes(x=dates,y=Forecasted_load)) ### almost the same in the years
p + geom_line()

x11()
h <- ggplot(data, aes(x=dates)) +  geom_line( aes(y=dam), color="blue") + geom_line( aes(y=Forecasted_load), color="red") + scale_y_continuous (name = "dam",sec.axis = sec_axis(~.*1, name="Gas Price"))
h








## Split the dataset: we consider only data from 2018 and 2019

data.18.19 <- data[which(data$X>=2019),]

summary(data.18.19)

data.18.19 <- data[,-1]

#### TODO: check if there is statistical difference between any energy source measured at 9 and 10 
#### from the boxplot it does not seem

x11()
boxplot(data.18.19[,2:10],las=2,col="gold")

x11()
boxplot(data.18.19[,15:26],las=2,col="gold")

x11()
boxplot(scale (x=data.18.19[,2:10],center=T,scale=F),las=2,col="gold")

x11()
boxplot(scale (data.18.19[,15:26],center=T,scale=F),las=2,col="gold")

#standardize the dataset
data.sd <- scale(data.18.19)
data2.sd <- data.frame(data.sd)

x11()
boxplot(data2.sd,las=2,col="gold")

#PCA on original data
pc.data2.sd<-princomp(data2.sd,scores=T)
pc.data2.sd
summary(pc.data2.sd)

load.data2.sd <- pc.data2.sd$loadings
load.data2.sd

#covariate da togliere

vector = c(1,2,3,4,5,6,7,8,9,10,11, 12, 13, 20, 21, 22, 23, 24, 25, 28) #tolte categoriche e ore 9
data2 <- data.18.19[, -vector]
summary(data2)

#standardize the dataset
data2.sd <- scale(data2)
data2.sd <- data.frame(data2.sd)

#PCA on original data
pc.data2.sd<-princomp(data2.sd,scores=T)
pc.data2.sd
summary(pc.data2.sd)

load.data2.sd <- pc.data2.sd$loadings
load.data2.sd

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for (i in 1:3)
  barplot(load.data2.sd[,i], ylim = c(-1,1), main=paste('Loadings PC'), i)
graphics.off()
### This PCA is a shit!!!

data.18.19 <- data[which(data$X<2020),]
data.18.19 <- data.18.19[,-1]
attach(data.18.19)

cov <- names(data.18.19)

regression <- lm(dam ~ gas_price + valley + peak + wind9 + thermal9 + self.consumption9 + pv9 + hydro9 + geothermal9 + Forecasted_load)
regression

summary(regression)

coef(regression)
vcov(regression)
residuals(regression)
fitted(regression)
