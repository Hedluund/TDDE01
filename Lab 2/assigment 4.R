
rm(list=ls())
library(ggplot2)
library(MASS)
library(COUNT)
library(fastICA)

set.seed(12345)

data <- read.csv2("NIRSpectra.csv")

# PCA - principle component analysis
# Task 1 below------ sum sdev= 0.707334
data1=data
data1$Viscosity=c()

# prcomp to ge loadings
result= prcomp(data1)

# sdev squared
lambda = result$sdev^2
percentVarians = sprintf("%2.3f",lambda/sum(lambda)*100)
screeplot(result)


plot(result$x[,1], result$x[,2], ylim=c(-2,2), xlim=c(0,2),
     xlab="PC1", ylab="PC2", main="Original PCA")

# 2 components needed

# rotation is the matrix of loadings
U = result$rotation
head(U)

#plotting the values in the pca:s, eg the new feature space
plot(result$x[,1], result$x[,2], ylim=c(-5,15))

# Some outliers in pc1

#Task 2 below----------------------


#PC2 is described by fewer features

#Task 3 below --------------------

set.seed(12345)
#ICA = independent component analysis
icaSolution = fastICA(data1, 2, alg.typ = "parallel", fun = "logcosh",
                      alpha = 1,method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE) 

K = icaSolution$K
W = icaSolution$W
wPrime = K %*% W


par(mfrow = c(1,2))
plot(U[,1], main="Traceplot, PC1")
plot(wPrime[,1], main = "Traceplot, ICA1")
plot(U[,2],main="Traceplot, PC2")
plot(wPrime[,2], main = "Traceplot, ICA2")

