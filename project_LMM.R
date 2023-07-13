### LMM


library(ggplot2)
library(GGally)
library(tidyverse)
library(patchwork)
library(lubridate, warn.conflicts = FALSE)
library(dplyr)
library(MVN)
library(car)

# importing libraries for LMM
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(plot.matrix)


#data <- read.table('data_w_temp.csv',header = T, sep=",")
data <- read.table('ml_data.csv',header = T, sep=",")
summary(data)
head(data)

mesi <- NULL

for (i in 1:dim(data)[1]) {
  anno <- as.integer(substr(data[i,1], 1, 4))
  mese <- (anno-2018) * 12 + data[i,14]
  mesi <- c(mesi,mese)
}

data$month <- mesi

dates <- ymd_hms(data$date)

# Visualizing dam electricity price

data = data.frame(dates, data[,2:30])  # dates with the right format for plotting time series

x11()
p <- ggplot(data,aes(x=dates,y=dam)) ### what's happened???????
p + geom_line()

data <- data[,2:30]


#fattore <- gl(nrow(data)/24, 24)

# Raggruppamento delle righe e calcolo delle medie
#medie <- sapply(split(data, fattore), colMeans)

# Visualizzazione delle medie calcolate
#print(medie)

data <- data[1:48120,]

data_day <- apply(data, 2, function(col) {
  tapply(col, rep(1:(43824/24), each=24, length.out=43824), mean)
})

rownames(data_day) <- 1:1826
data_day <- data.frame(data_day)

x11()
plot(data$dam,type='l')

# deleting not relevant features

data_day <- data_day[,-c(2,3,4,5,6,7,8,9,11,21,22,23,24,25)]

data_day$weekday <- ifelse(data_day$weekday==0 | data_day$weekday==6,1,0) #0:workday


flst <- list(data_day$month, data_day$weekday)
tMn <- tapply(data_day$dam, flst, FUN = mean)
tMn

x11()
plot(1:60, tMn[,1], type = "l", col = "blue", lwd = 2, xlab = "X", ylab = "Y")

# Sovrapposizione della seconda curva
lines(1:60, tMn[,2], type = "l", col = "red", lwd = 2)

lines(1:60, tMn[,1] - tMn[,2], type = "l", col = "green", lwd = 2)

## We confirm what we observe in the plot

## Box-plots for visual acuity by treatment and time
bw1 <- bwplot(dam ~ month | weekday,
              data = data_day)
#xlims <- c("Base", "4\nwks", "12\nwks", "24\nwks", "52\nwks")
update(bw1, pch = "|")


lm <- lm(dam ~ weekday + gas_price + Forecasted_load + month, data = data_day)
summary(lm)

lm$coefficients

x11()
par(mfrow=c(2,2))
plot(lm)

shapiro.test((lm$residuals))


#LMM

fm16.1mer <- lmer(dam ~ weekday + gas_price + Forecasted_load  + (1|month),
                  data = data_day)

summary(fm16.1mer)

confint(fm16.1mer,oldNames=TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(fm16.1mer) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(fm16.1mer)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(fm16.1mer), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(fm16.1mer))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm16.1mer))
sigma2_b


## the marginal variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A) --> s^2*(I_N) is the error part, s^2*(A*A) is the random effect part
V[3:6, 3:6]  #-> V is a block-diagional matrix, the marginal var-cov matrix

# visualization of the first 20 rows/columns
plot(as.matrix(V[1:20,1:20]), main = 'Marginal estimated Var-Cov matrix of Y')
## we visualize the first 20 rows/columns of the matrix
plot(as.matrix(SigmaErr[1:20,1:20]), main = 'Conditional estimated Var-Cov matrix of Y')

# Another way to interpret the variance output is to note percentage of the subject variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC), because it is also an estimate of the within 
# cluster correlation.

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 51% is very high!

## visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i for i=1,...,234
dotplot(ranef(fm16.1mer, condVar=T))

# Diagnostic plots 
#--------------------
# 1) Assessing Assumption on the within-group errors
x11()
plot(fm16.1mer)  ## Pearson and raw residuals are the same now

x11()
qqnorm(resid(fm16.1mer))
qqline(resid(fm16.1mer), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(fm16.1mer)$month), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(fm16.1mer)$month), col='red', lwd=2)

#random slope
fm16.2mer <- lmer(dam ~ gas_price + Forecasted_load  + (1+weekday|month),
                  data = data_day, control=lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5)))

summary(fm16.2mer)
confint(fm16.2mer,oldNames=TRUE)

#sig02 covariance between the two random effects

vcovb <- vcov(fm16.2mer) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(fm16.2mer)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(fm16.2mer), comp = c("Variance", "Std.Dev."))


## Let's compute the conditional and marginal var-cov matrix of Y
sgma <- summary(fm16.2mer)$sigma

A <- getME(fm16.2mer, "A") # A : N*2 x n
I.n <- Diagonal(ncol(A)) # IN: n x n

## the conditional variance-covariance matrix of Y (diagonal matrix)
SigmaErr = sgma^2 * (I.n)
SigmaErr[3:6, 3:6]  ## visualization of individual 2
# Conditioned to the random effects b_i, we observe the var-cov of the errors
# that are independent and homoscedastic

plot(as.matrix(SigmaErr[1:20,1:20]), main = 'Conditional estimated Var-Cov matrix of Y')

## the marginal variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A)
V[3:6, 3:6]  #-> V is a block-diagional matrix, the marginal var-cov matrix

# visualization of the first 20 rows/columns
plot(as.matrix(V[1:20,1:20]), main = 'Marginal estimated Var-Cov matrix of Y')


# PVRE
#--------------------
# In this case the variance of random sigma2_R effects represents the mean random 
# effect variance of the model and is given by
# sigma2_b = Var(b0,b1) = sigma2_b0 + 2Cov(b0,b1)*mean(w) + sigma2_b1*mean(w^2)
# See equation (10) in Johnson (2014), Methods in Ecology and Evolution, 5(9), 944-946.
sigma2_eps <- as.numeric(get_variance_residual(fm16.2mer))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm16.2mer)) # 49.933917 + 0.074552*mean(armd$time^2) +2*0.143*7.06639*0.27304*mean(armd$time)
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 72% is very high!

## visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i, b_1i for i=1,...,234
dotplot(ranef(fm16.2mer, condVar=T))

# Comparing models
#------------------
# The anova function, when given two or more arguments representing fitted models,
# produces likelihood ratio tests comparing the models.
anova(fm16.1mer, fm16.2mer)


# The p-value for the test is essentially zero -> we prefer fm16.2mer


# Diagnostic plots 
#--------------------
# 1) Assessing Assumption on the within-group errors
x11()
plot(fm16.2mer)

x11()
qqnorm(resid(fm16.2mer))
qqline(resid(fm16.2mer), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(fm16.2mer)$month[,1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(fm16.2mer)$month[,1]), col='red', lwd=2)

x11()
qqnorm(unlist(ranef(fm16.2mer)$month[,2]), main='Normal Q-Q Plot - Random Effects on Slope')
qqline(unlist(ranef(fm16.2mer)$month[,2]), col='red', lwd=2)


x11()
par(mfrow=c(1,2))
plot(school$escs[school$gender==0], school$achiev[school$gender==0],col='blue',
     xlab='escs', ylab='achievement',ylim=c(-5,30),main='Data and regression lines for females')
abline(10.0546535,1.6790886, col='red', lw=6)          

for(i in 1:50){
  abline(coef(lmm2)$school_id[i,1], coef(lmm2)$school_id[i,3])
}

## MALES
plot(school$escs[school$gender==1], school$achiev[school$gender==1],col='blue',
     xlab='escs', ylab='achievement',ylim=c(-5,30),main='Data and regression lines for males')
abline(10.02507-0.91180,1.96618, col='red', lw=6)  

for(i in 1:50){
  abline(coef(lmm2)$school_id[i,1] + coef(lmm2)$school_id[i,2], coef(lmm2)$school_id[i,3])
}

