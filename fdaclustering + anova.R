library(fda)
library(KernSmooth)
library(fields)
library(fdacluster)
library(lubridate, warn.conflicts = FALSE)


rm(list=ls())
graphics.off()

## Read data:


data <- read.table('data_w_temp.csv', header = TRUE, sep=",")
head(data)
dates <- ymd_hms(data$date)
data = data.frame(dates, data[,-1])

df1 = data.frame(data$dam)

df <- matrix(df1$data.dam[-46681], nrow = 1945, ncol = 24, byrow = TRUE)
df <- as.data.frame(df)


rows <- floor(nrow(df) / 30)

# Creazione del nuovo dataframe
new_df <- data.frame(matrix(NA, nrow = rows, ncol = 24))

# Raggruppamento delle righe e calcolo della media per ogni gruppo
for (i in 1:rows) {
  ri <- (i - 1) * 30 + 1
  rf <- min(i * 30, nrow(df))
  new_df[i, ] <- colMeans(df[ri:rf, ])
}




df <- t(new_df) 
x <- seq(1,24,by=1) # set abscissas properly

N <- dim(df)[2]  # Useful for PCA




nbasis <- 5:16 # range of possible value to
m <- 5    # set spline order
gcv <- matrix(0, nrow = length(nbasis), ncol = dim(df)[2])
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(c(x[1],x[length(x)]), nbasis[i], m)
  for(j in 1:dim(df)[2]){
  gcv[i,j] <- smooth.basis(x, as.numeric(df[,j]), basis)$gcv
  }
}
meangcv <- rowMeans(gcv)

par(mfrow=c(1,1))
plot(nbasis,meangcv)
nbasis.opt <- nbasis[which.min(meangcv)] # Optimal value
nbasis.opt
abline(v=nbasis[which.min(meangcv)],col='red')

nbasis.opt <- 12

nbasis <- nbasis.opt 
m <- 5
degree <- m-1
N <- dim(df)[2]  # Useful for PCA
## only one curve
matplot(x, df,type='l')
basis <- create.bspline.basis(rangeval=c(x[1],x[length(x)]),nbasis=nbasis,norder=m)
df.fun <- Data2fd(y = df,argvals = x,basisobj = basis)
plot.fd(df.fun)



y.spline <- smooth.basis(argvals=x, y=df[,1], fdParobj=basis)
y.spline0 <- eval.fd(x, y.spline$fd) #  the curve smoothing the data
y.spline1 <- eval.fd(x, y.spline$fd, Lfd=1) # first derivative
y.spline2 <- eval.fd(x, y.spline$fd, Lfd=2) # second derivative

rappinc1 <- (df[,1][3:N]-df[,1][1:(N-2)])/(x[3:N]-x[1:(N-2)])
rappinc2 <- ((df[,1][3:N]-df[,1][2:(N-1)])/(x[3:N]-x[2:(N-1)])-(df[,1][2:(N-1)]-df[,1][1:(N-2)])/(x[2:(N-1)]-x[1:(N-2)]))*2/(x[3:(N)]-x[1:(N-2)])


plot(x,df[,1],xlab="t",ylab="observed data")
points(x,y.spline0 ,type="l",col="blue",lwd=2)

plot(x[2:(N-1)],rappinc1,xlab="t",ylab="1st order finite differences",type="l")
points(x,y.spline1 ,type="l",col="blue",lwd=2)

k <- 2 # set the number of clusters


y0 <- t(eval.fd(x, df.fun, Lfd=0)) # evaluations of the functions or derivatives after smoothing
y1 <- t(eval.fd(x, df.fun, Lfd=1)) # evaluations of original functions (matrix with 1 row for each function)



# set.seed(1)
# fdahcl <- fdahclust(
#   x=x, y=y0, n_clusters = 2, 
#   warping_class = 'srsf', 
#   metric = 'l2',  # similarity computed as the cosine between the first derivatives (correlation)
#   centroid_type = 'mean',
#   linkage_criterion = 'complete',
#   #seeds = c(1,21) # you can give a little help to the algorithm...
# )
# plot(fdahcl, type =  "amplitude")
# 
# id <- which.min(fdahcl$silhouettes)
# 
# clusters <- fdahcl$memberships

clusters <- rep(1:2, each = 45,length.out = 64)



df1 <- data$dam
df <- matrix(df1[-(46081:46681)], nrow = 64, ncol = 720, byrow = TRUE)
df <- rowMeans(df)

head(df)
plot(df)

df <- log(df)


df <- cbind(df,clusters)
df <- as.data.frame(df)

head(df)
names(df)[1] <- 'dam'


data <- df

p <- 1
factor1 <- factor(data$clusters) 

x <- log(data$dam)
# ******************************************************************************

# boxplot of data divided by labels:
plot(factor1, x, xlab='labels', ylab='x', col='grey85', main='Dataset')

# look at if there are some differences in terms of variability between the different groups. 

# set important quantities:
n <- dim(data)[1]         # number of observations
ng <- table(factor1)      # number of observations in each group
treat <- levels(factor1)  # levels of the treatment
g <- length(treat)        # number of levels/groups/labels


## We are building this model:
## \[ x_{ij} = \mu + \tau_i + \varepsilon_{ij} \quad \quad \varepsilon_{ij} \sim N(0, \sigma^2) \]
### where:
### $\mu$ : overall mean of the x; $\tau_i$: effect of the treatment $i$; $\varepsilon_{ij}$: additive Gaussian random noise.
## Constraint: \[\sum_{i=1}^{g} \tau_i n_i=0\]

## We want to perform this test to see if the treatment given by the labels has an effect on x:
# \[ H_0: \tau_1=\tau_2=\dots=\tau_g=0 \quad vs \quad H_1: (H_0)^C \]
### namely:
### $H_0$: the treatment has no effect;
### $H_1$: at least one treatment has an effect;

## Model assumptions:
### - Gaussian distribution of the error;
### - Homoschedasticity.


## Verify assumption of the model:
## - Normality in each group:

## we are in ANOVA setting, so we perform $g$ shapiro tests, one for each group:

pvalue <- NULL
for (i in 1:g) {
  pval <- shapiro.test(x[factor1==treat[i]])$p
  pvalue <- c(pvalue, pval)
}
pvalue

## Given the pvalues of the shapiro tests ... I cannot reject the gaussianity
## hypothesis for each group at level 5%

## - Same covariance structure
#
## I can perform the Bartlett test (that relies on Gaussianity assumption) to check homogeneity of variances. 
## Namely, the test I'm performing is the following:
## \[ H_0: \sigma_1 = \sigma_2 = \dots = \sigma_g \quad vs \quad H_1: \exists i,j s.t. \sigma_i \neq \sigma_j\]

bartlett.test(x, factor1)

## Given the pvalues of the bartlett test i cannot reject the Homoschedasticity hypothesis at level 5%


# Now I can perform the One-Way ANOVA:
fit <- aov(x ~ factor1)     # aov( variable of interest ~ treatment )
summary(fit)

## Given the pvalue of the F-statstic we have statstical evidence that
## at least one tau_i is different from 0.

#STIME:
## Estimate variances
W <- sum(fit$residuals^2)  # SS_res
n <- dim(data)[1] ## check!!
var <- W/(n-g)     # SS_res/gdl(res)   # controlla n!!
var
## Estimate the great mean mu:
m <- mean(data[,1])
m
## Estimate tau.i:
tau1  <- mean(data[factor1==1,1]) - m  # tau.1 #****************************************
tau2  <- mean(data[factor1==2,1]) - m  # tau.2 #****************************************
# tau3  <- mean(data[factor1=='GRUPPO3',1]) - m  # tau.3 #solo se hai 3 gruppi nel factor ****************************************
tau <- cbind(tau1,tau2)
tau 
## Point-wise estimate of the mean:
m1 <- m + tau1
m2  <- m + tau2
# m3  <- m + tau3
mu <- cbind(m1,m2)
mu 

g <- length(levels(factor1))
k <- g*(g-1)/2    # +1 se chiede sia media che varianza number of comparisons
alpha <- 0.05     # overall level # ******************************************************************************

Mediag <- tapply(x, factor1, mean) 
SSres <- sum(residuals(fit)^2)
S <- SSres/(n-g) # controlla n!!
treat <- levels(factor1)

ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * 
                         sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * 
                         sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * 
                                         sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * 
                                         sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
  }}












