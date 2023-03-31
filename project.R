### Forecasting Electicity price ####

data <- read.table('ml_data.csv',header = T, sep=",")

summary(data)
head(data)

## Split the dataset: we consider only data from 2018 and 2019

data.18.19 <- data[which(data$X<2020),]

summary(data.18.19)

data.18.19 <- data[,-1]

x11()
boxplot(data1, col='gold')

#### TODO: check if there is statistical inference between any energy source measured at 9 and 10 
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

#PCA on original data
pc.data2.sd<-princomp(data2.sd,scores=T)
pc.data2.sd
summary(pc.data2.sd)

load.data2.sd <- pc.data2.sd$loadings
load.data2.sd

#covariate da togliere

vector = c(1, 12, 13, 14, 21, 22, 23, 24, 25, 26, 28) #tolte categoriche e ore 9
data2 <- data.18.19[, -vector]

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
for (i in 1:3)barplot(load.data2.sd[,i], ylim = c(-1,1), main=paste('Loadings PC'), i, sep='')

### This PCA is a shit!!!
