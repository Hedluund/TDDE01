rm(list=ls())

data = read.csv2("tecatorCsv.csv")

library(MASS)
library(Metrics)
library(glmnet)

#yes, it locks like it fits well to a linear model. But the lines seems to have some different slope
# Which might indicate that it can not be described by a linear model.

set.seed(12345)
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]
test=test[c(1:107),]
#Plockar ut alla observationer 
channelData = data[, 2:102]


# Creating different models with different levels of polynoms.
# Model_1 describes the moisture with a first degree polynome (y=b1+b2*x).Model_2 is quadratic (y=b1+b2*x+b3*X^2)
Model_1=lm(train$Moisture~poly(train$Protein,1),data=train)
Model_2=lm(train$Moisture~poly(train$Protein,2),data=train)
Model_3=lm(train$Moisture~poly(train$Protein,3),data=train)
Model_4=lm(train$Moisture~poly(train$Protein,4),data=train)
Model_5=lm(train$Moisture~poly(train$Protein,5),data=train)
Model_6=lm(train$Moisture~poly(train$Protein,6),data=train)

pred1=predict.lm(Model_1, test)
pred2=predict.lm(Model_2, test)
pred3=predict.lm(Model_3, test)
pred4=predict.lm(Model_4, test)
pred5=predict.lm(Model_5, test)
pred6=predict.lm(Model_6, test)

pred1_train=predict.lm(Model_1, train)
pred2_train=predict.lm(Model_2, train)
pred3_train=predict.lm(Model_3, train)
pred4_train=predict.lm(Model_4, train)
pred5_train=predict.lm(Model_5, train)
pred6_train=predict.lm(Model_6, train)

#mse computes mean squared error between two vectors.
mse1=mse(test$Moisture, pred1)
mse2=mse(test$Moisture, pred2)
mse3=mse(test$Moisture, pred3)
mse4=mse(test$Moisture, pred4)
mse5=mse(test$Moisture, pred5)
mse6=mse(test$Moisture, pred6)

mse1train=mse(train$Moisture, pred1_train)
mse2train=mse(train$Moisture, pred2_train)
mse3train=mse(train$Moisture, pred3_train)
mse4train=mse(train$Moisture, pred4_train)
mse5train=mse(train$Moisture, pred5_train)
mse6train=mse(train$Moisture, pred6_train)

validationMSEVector=c(mse1, mse2, mse3, mse4, mse5, mse6)
trainMSEVector=c(mse1train, mse2train, mse3train, mse4train, mse5train, mse6train)

plot(validationMSEVector, type="l")
plot(trainMSEVector, type="l")

set.seed(12345)
allVariablesModel = glm(channelData$Fat~. ,data=channelData)
set.seed(12345)

#stepAIC done backward. Starts with all variables and takes away some variables and checks if the
# results gets better.

stepAICResult = stepAIC(allVariablesModel, direction = "backward")

#stepAICResult$anova

# 64 coefficients were used 


# #5 ridge regression -----------------------------------------------------

#scale centers and normalize the data
covariates = scale(channelData[,1:100])
response = scale(channelData[,101])

set.seed(12345)
#Alpha = 0 gives a ridgeRegression and Alpha = 1 gives LassoRegression
ridgeRegResults = glmnet(as.matrix(covariates), response, alpha = 0, family = "gaussian")

plot(ridgeRegResults,xvar="lambda", label=TRUE)

lassoResults = glmnet(as.matrix(covariates), response, alpha = 1, family = "gaussian")

lambdaVector = c(0,lassoResults$lambda)

plot(lassoResults,xvar="lambda", label=TRUE)

crossValidationLasso = cv.glmnet(as.matrix(covariates),lambda = lambdaVector, response, alpha = 1, family = "gaussian")

crossValidationLasso$lambda.min
plot(crossValidationLasso)
optimalCoefGivenLambda=coef(crossValidationLasso,s="lambda.min")