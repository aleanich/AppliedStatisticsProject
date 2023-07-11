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

data.18.19 <- data[which(data$date<2020),]
dates <- ymd_hms(data.18.19$date)
data.18.19 = data.frame(dates, data.18.19[,2:30])

data.18=data.18.19[1:8760,]
x11()
p <- ggplot(data.18,aes(x=dates,y=dam)) ### what's happened???????
p + geom_line()


# Visualizing other variables

#Swiss_import and export

x11()
q <- ggplot(data,aes(x=dates)) +  geom_line( aes(y=Swiss_import), color="blue") + geom_line( aes(y=Swiss_export),size = 1, color="green") + scale_y_continuous (name = "S_I",sec.axis = sec_axis(~.*1, name="S_E"))
q 

x11()
q <- ggplot(data,aes(x=dates,y=Swiss_export))
q + geom_line()

#Swiss_import and dam

x11()
h <- ggplot(data, aes(x=dates)) +  geom_line( aes(y=dam), color="blue") + geom_line( aes(y=gas_price),size = 1, color="green") + scale_y_continuous (name = "dam",sec.axis = sec_axis(~.*1, name="Gas Price"))
h

#France_import and export

x11()
q <- ggplot(data,aes(x=dates,y=France_import))
q + geom_line()

x11()
q <- ggplot(data,aes(x=dates,y=France_export))
q + geom_line()

x11()
q <- ggplot(data,aes(x=dates)) +  geom_line( aes(y=France_import), color="blue") + geom_line( aes(y=France_export),size = 1, color="green") + scale_y_continuous (name = "S_I",sec.axis = sec_axis(~.*1, name="S_E"))
q 


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

data <- read.table('ml_data.csv',header = T, sep=",")

data.18.19 <- data[which(data$date<2020),]

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

vector = c(1,2,3,4,5,6,7,8,9,10,11, 12, 13, 20, 21, 22, 23, 24, 25, 26, 27, 28) #tolte categoriche e ore 9
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



data.18.19 <- data[which(data$date<2020),]

dam18 <- data.18.19[which(data.18.19$date<2019),2]
dam19 <- data.18.19[which(data.18.19$date>=2019),2]

#shapiro.test(dam19[1:5000])

mean(dam18)
mean(dam19)

sd(dam18)
sd(dam19)

x11()
par(mfrow=c(1,2))
boxplot(dam18,col='gold',ylim=c(0,150))
boxplot(dam19,col='gold',ylim=c(0,150))

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



data.18.19 <- data[which(data$date<2020),]

data.18.19.meandam <- data.18.19[,c(2,14)]
summary(data.18.19.meandam)
data.18.19.meandam.dam <- tapply(data.18.19.meandam$dam, rep(1:17520/24, each=24, length.out=17520), mean)
data.18.19.meandam.month <- tapply(data.18.19.meandam$month, rep(1:17520/24, each=24, length.out=17520), mean)
summary(data.18.19.meandam.month)
dam <- data.18.19.meandam.dam
month <- data.18.19.meandam.month
df = data.frame(dam,month,row.names = c(1:730))


inverno <- which( df$month==1 | df$month==2 | df$month==12)
primavera <- which( df$month==3 | df$month==4 | df$month==5)
estate <- which( df$month==6 | df$month==7 | df$month==8)
autunno <- which( df$month==9 | df$month==10 | df$month==11)

df[inverno,2] <- "inverno"
df[estate,2] <- "estate"
df[primavera,2] <- "primavera"
df[autunno,2] <- "autunno"

stagione <- factor(x=df[,2],c("inverno","estate","primavera","autunno"))
stagione

df[,2] <- stagione

stagioni <- c("inverno","estate","primavera","autunno")

Mediag  <- tapply(df$dam, df$month, mean)
Sdg  <- tapply(df$dam, df$month, sd)
ng <- tapply(df$dam, df$month,length)

for (i in 1:4) {
    s <- which(df$month==stagioni[i] & abs((df$dam - Mediag[i])/Sdg[i]) > 2.5)
    if (length(s) > 0) {
      df <- df[-s,]
    }
    
}

shapiro.test(df[inverno,1])
shapiro.test(df[autunno,1])
shapiro.test(df[primavera,1])
shapiro.test(df[estate,1])

bartlett.test(df$dam, df$month)

fit <- aov(df$dam ~ df$month)
summary(fit)

n=730
g=4
ng <- tapply(df$dam,df$month,length)
k <- g*(g-1)/2
alpha= 0.05

Mediag  <- tapply(df$dam, df$month, mean) # group-wise means
SSres <- sum(residuals(fit)^2)
S <- SSres/(n-g)

# CI for all the differences
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(stagione[i],"-",stagione[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])))))
  }}

x11()
par(mfrow=c(1,2))
plot(df$month, df$dam, xlab='stagione', ylab='dam', col = rainbow(6), las=2)

h <- 1
plot(c(1,g*(g-1)/2),range(ICrange), pch='',xlab='pairs treat', ylab='Conf. Int. tau weight')
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    ind <- (i-1)*g-i*(i-1)/2+(j-i)
    lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
    points(h, ICrange[ind,1], col=rainbow(6)[j], pch=16); 
    points(h, ICrange[ind,2], col=rainbow(6)[i], pch=16); 
    h <- h+1
  }}
abline(h=0)

### Some plot in order to spot some relationships

data1819.x <- bcPower(data.18.19[,30], 2)

x11()
plot(data.18.19$Forecasted_load,data.18.19$dam)


x11()
plot(data1819.x,data.18.19$dam,xlim=c(0,2500))







### Linear Regression 

data <- read.table('ml_data.csv',header = T, sep=",")
data.18.19 <- data[which(data$date<2020),]

cov <- c(2,11,12,14,15,16,17,18,19,20,30)

data.18.19 <- data.18.19[,cov]

## Try different regression

# - hours and month as categorical variables

hours <- factor(x=data.18.19[,3],c(0:23))
month <- factor(x=data.18.19[,4],c(1:12)) 

data.18.19$hour <- hours
data.18.19$month <- month

regression <- lm(data.18.19$dam ~ data.18.19$gas_price + data.18.19$Forecasted_load + data.18.19$hour + data.18.19$geothermal10 + data.18.19$pv10 + data.18.19$hydro10 + data.18.19$self.consumption10 + data.18.19$thermal10 + data.18.19$wind10)
regression
summary(regression)

x11()
plot(regression$residuals)

# - 24 different regressions: each one for every hour

data0 <- data.18.19[which(data.18.19$hour==0),]
regression0 <- lm(data0$dam ~ data0$gas_price + data0$Forecasted_load + data0$geothermal10 + data0$pv10 + data0$hydro10 + data0$self.consumption10 + data0$thermal10 + data0$wind10)
summary(regression0)

x11()
plot(regression0$residuals)
summary(regression0$residuals)

data1 <- data.18.19[which(data.18.19$hour==1),]
regression1 <- lm(data1$dam ~ data1$gas_price + data1$Forecasted_load + data1$geothermal10 + data1$pv10 + data1$hydro10 + data1$self.consumption10 + data1$thermal10 + data1$wind10)
summary(regression1)

x11()
plot(regression1$residuals)

data12 <- data.18.19[which(data.18.19$hour==12),]
regression12 <- lm(data12$dam ~ data12$gas_price + data12$Forecasted_load + data12$geothermal10 + data12$pv10 + data12$hydro10 + data12$self.consumption10 + data12$thermal10 + data12$wind10)
summary(regression12)

x11()
plot(regression12$residuals)

# - trying a sort of recursive linear regression

data1 <- data.18.19[which(data.18.19$hour==1),]
data1$past_dam <- regression0$fitted.values

regression1_rec <- lm(data1$dam ~ data1$gas_price + data1$Forecasted_load + data1$geothermal10 + data1$pv10 + data1$self.consumption10 + data1$past_dam)
summary(regression1_rec)



### Covariate trasformate

data <- read.table('ml_data.csv',header = T, sep=",")
data.18.19 <- data[1:17519,]
data.18.19 <- data.18.19[,-1]


#tolgo le covariate meno significative
data.18.19<-data.18.19[,-c(2,3,4,5,6,7,8,9)]
summary(data.18.19)

#hydro, pv, self.consumption
data.18.19$geothermal=(data.18.19$geothermal10+data.18.19$geothermal9)/2
data.18.19$hydro=(data.18.19$hydro10+data.18.19$hydro9)/2
data.18.19$pv=(data.18.19$pv10+data.18.19$pv9)/2
data.18.19$self.consuption=(data.18.19$self.consumption9+data.18.19$self.consumption10)/2
data.18.19<-data.18.19[,-c(6,7,8,9,10,11,12,13,14,15,16,17)]

#pick e valley
data.18.19$range=data.18.19$peak-data.18.19$valley
data.18.19<-data.18.19[,-c(6,7)]
data.18.19<-data.18.19[,-c(6)]

hours <- factor(x=data.18.19[,3],c(0:23))
month <- factor(x=data.18.19[,5],c(1:12)) 

data.18.19$hour <- hours
data.18.19$month <- month


regression <- lm(data.18.19$dam ~ data.18.19$gas_price + data.18.19$Forecasted_load + data.18.19$hour + data.18.19$geothermal + data.18.19$pv + data.18.19$hydro + data.18.19$self.consuption + data.18.19$range)
regression

summary(regression)

x11()
plot(regression$residuals)


data0 <- data.18.19[which(data.18.19$hour==0),]
regression0 <- lm(data0$dam ~ data0$gas_price + data0$Forecasted_load + data0$geothermal + data0$pv + data0$hydro + data0$self.consuption + data0$range)
summary(regression0)

data1 <- data.18.19[which(data.18.19$hour==1),]
data1$past_dam <- regression0$fitted.values

regression1 <- lm(data1$dam ~ data1$gas_price + data1$Forecasted_load + data1$geothermal + data1$pv + data1$hydro + data1$self.consuption + data1$range + data1$past_dam)
summary(regression1)

