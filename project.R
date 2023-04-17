### Forecasting Electricity price ###

library(ggplot2)
library(GGally)
library(tidyverse)
library(patchwork)
library(lubridate, warn.conflicts = FALSE)
library(dplyr)
library(MVN)
library(car)

data <- read.table('ml_data.csv',header = T, sep=",")
summary(data)
head(data)

dates <- ymd_hms(data$date)

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

data.18.19 <- data[which(data$date>=2019),]

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

data.18.19 <- data[which(data$date<2020),]
data.18.19 <- data.18.19[,-1]
attach(data.18.19)

cov <- names(data.18.19)

###

# regression <- lm(dam ~ gas_price + valley + peak + wind9 + thermal9 + self.consumption9 + pv9 + hydro9 + geothermal9 + Forecasted_load)
# regression
# 
# summary(regression)
# 
# coef(regression)
# vcov(regression)
# residuals(regression)
# fitted(regression)

### Using ANOVA in order to spot differences in dam between years -> seasons -> months -> weeks -> weekdays

dates <- ymd_hms(data.18.19$date)

# Visualizing dam electricity price

data = data.frame(dates, data.18.19[,2:30])  # dates with the right format for plotting time series

x11()
p <- ggplot(data,aes(x=dates,y=dam)) ### what's happened???????
p + geom_line()



dam18 <- data.18.19[which(data.18.19$date<2019),2]
dam19 <- data.18.19[which(data.18.19$date>=2019),2]

shapiro.test(dam19[1:5000])

mean(dam18)
mean(dam19)

sd(dam18)
sd(dam19)

x11()
par(mfrow=c(1,2))
boxplot(dam18)
boxplot(dam19)

# groups are seasons: 1 winter, 2 spring, 3 summer, 4 autumn

s1 <- which(data.18.19$hour==12 & data.18.19$month==1 | data.18.19$hour==12 & data.18.19$month==2 | data.18.19$hour==12 &data.18.19$month==12)
s2 <- which(data.18.19$hour==12 & data.18.19$month==3 | data.18.19$hour==12 & data.18.19$month==4 | data.18.19$hour==12 & data.18.19$month==5)
s3 <- which(data.18.19$hour==12 & data.18.19$month==6 | data.18.19$hour==12 & data.18.19$month==7 | data.18.19$hour==12 & data.18.19$month==8)
s4 <- which(data.18.19$hour==12 & data.18.19$month==9 | data.18.19$hour==12 & data.18.19$month==10 | data.18.19$hour==12 & data.18.19$month==11)


s=c(s1,s2,s3,s4)

x11()
par(mfrow=c(1,4))

boxplot(data.18.19[s1,2],col='gold',ylim=c(0,120))
boxplot(data.18.19[s2,2],col='gold',ylim=c(0,120))
boxplot(data.18.19[s3,2],col='gold',ylim=c(0,120))
boxplot(data.18.19[s4,2],col='gold',ylim=c(0,120))

Ps <- NULL
mvn.test <- shapiro.test(scale(data.18.19[s1,2]))
Ps <- c(Ps, mvn.test$multivariateNormality$`p value`)
mvn.test <- mvn(data = data.18.19[s2,2])
Ps <- c(Ps, mvn.test$multivariateNormality$`p value`)
mvn.test <- mvn(data = data.18.19[s3,2])
Ps <- c(Ps, mvn.test$multivariateNormality$`p value`)
mvn.test <- mvn(data = data.18.19[s4,2])
Ps <- c(Ps, mvn.test$multivariateNormality$`p value`)

Ps


x11()
qqnorm(data.18.19[s1,2])
qqline(data.18.19[s1,2],col='red')

# trying to remove some outliers 

#d2 <- mahalanobis(data.18.19[s1,2], mean(data.18.19[s1,2]), cov(data.18.19[s1,2]))

data_norm <- abs((data.18.19[s1,2] - mean(data.18.19[s1,2]))/sd(data.18.19[s1,2]))
d1 <- data.18.19[s1,2][which(data_norm<2.5)]
summary(d1)

shapiro.test(d1)

x11()
qqnorm(d1)
qqline(d1,color='red')

data_norm <- abs((data.18.19[s2,2] - mean(data.18.19[s2,2]))/sd(data.18.19[s2,2]))
d2 <- data.18.19[s2,2][which(data_norm<2.5)]
summary(d2)

shapiro.test(d2)

x11()
qqnorm(d2)
qqline(d2,color='red')

data_norm <- abs((data.18.19[s3,2] - mean(data.18.19[s3,2]))/sd(data.18.19[s3,2]))
d3 <- data.18.19[s3,2][which(data_norm<2.5)]
summary(d3)

shapiro.test(d3)

x11()
qqnorm(d3)
qqline(d3,color='red')

data_norm <- abs((data.18.19[s4,2] - mean(data.18.19[s4,2]))/sd(data.18.19[s4,2]))
d4 <- data.18.19[s4,2][which(data_norm<2.5)]
summary(d4)

shapiro.test(d4)

x11()
qqnorm(d4)
qqline(d4,color='red')


lambda.x <- powerTransform(data.18.19[s1,2])
lambda.x

data1819.x <- bcPower(data.18.19[s1,2], lambda.x$lambda)

shapiro.test(data.18.19[s1,2])
shapiro.test(data1819.x)


