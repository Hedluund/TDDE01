rm(list=ls())
library(ggplot2)
library(MASS)
library(COUNT)

missclass=function(X,Y){
  size=length(X)
  rate=(1-sum(diag(table(X,Y)))/size)
  return (rate)
}

data = read.csv("australian-crabs.csv")
set.seed(12345)

colourSex = data$sex

qplot(data$CL,data$RW,data=data ,colour = colourSex)

# It seems resonable because you can draw a distinct line between the colors and get a pretty accurate
# separation.

# task 2 below --------------------------------------------

# LDA to find a line to seperate two or more classes
LDAModel = lda(data$sex~data$CL+data$RW,data=data)

predictionsSex = predict(LDAModel, data)

predictionsInMaleOrFemale = data.frame(predictionsSex$class)

missClassificationRate = missclass(data$sex,predictionsInMaleOrFemale$predictionsSex.class)

# The dots balck dots are females and red dots are male
plot(predictionsSex$posterior[,"Female"],(data$CL+data$RW),col= c(predictionsInMaleOrFemale$predictionsSex.class))

colorPredSex = predictionsInMaleOrFemale$predictionsSex.class

qplot(data$CL,data$RW,data=data , main = "Predictions without prior" ,xlab="claw length", ylab = "Rear width" ,colour = colorPredSex )

# task 3 below --------------------------------------------
# prior of male set to 0.9

LDAModelPrior0.9 = lda(data$sex~data$CL+data$RW,data=data,prior=c(0.1 , 0.9))

predictionsSexPrior0.9 = predict(LDAModelPrior0.9, data)

predictionsInMaleOrFemalePrior0.9 = data.frame(predictionsSexPrior0.9$class)

missClassificationRatePrior0.9 = missclass(data$sex, predictionsInMaleOrFemalePrior0.9$predictionsSexPrior0.9.class)

plot(predictionsSexPrior0.9$posterior[,"Female"],(data$CL+data$RW),col= c(predictionsInMaleOrFemalePrior0.9$predictionsSexPrior0.9.class))

colourPredSexPrior = predictionsInMaleOrFemalePrior0.9$predictionsSexPrior0.9.class

qplot(data$CL,data$RW,data=data, main = "Predictions with prior male = 0.9 and female = 0.1" ,xlab="claw length", ylab = "Rear width",colour = colourPredSexPrior)



# task 4  below -------------------------------------------

glmModel = glm(data$sex~data$CL+data$RW, data=data, family = "binomial")

##coefficients function extracts coeff from model 
beta0 = coefficients(glmModel)[1]
beta1 = coefficients(glmModel)[2]
beta2 = coefficients(glmModel)[3]

predictGlm = predict(glmModel, newdata = data, type = "response")


classifiedPredicts = c()

for(prob in predictGlm){
  if(prob>=0.5){
    classifiedPredicts= c(classifiedPredicts,"Male")
  }else{
    classifiedPredicts=c(classifiedPredicts,"Female")
  }
}


missClassGlm = missclass(data$sex, classifiedPredicts)

#creates a vector of 200 numbers from 
lineVector = seq(10,59.75, by=0.25)
predLine = c()

for( i in lineVector){
  predLine = c(predLine, -((beta0+i*beta1)/beta2))
}

plotDecisionLine = ggplot(data.frame(predictGlm),aes(x=data$CL, y=data$RW, colour = classifiedPredicts)) + geom_point(size=2, shape=1) + labs(x = "Carapace Length", y = "Rear Width", colour = "Sex") +
  geom_line(aes(x = lineVector, y = predLine)) 
plot(plotDecisionLine)





