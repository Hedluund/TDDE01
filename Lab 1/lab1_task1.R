rm(list=ls())
library(kknn)
missclass=function(X,Y){
  size=length(X)
  rate=(1-sum(diag(table(X,Y)))/size)
  return (rate)
}

data = read.csv2("spambase.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

correctAnswers = test$Spam
correctAnswersTrain = train$Spam


#glm tar fram koefficienter från train med target på spam och sen använder alla förutom spam som predictors

fit = glm(Spam~. , data=train )

# predict with the help of our model "fit" and predicting the data set in "newdata"
# type = "class" gives back the class type it got, type = "response" gives a number between 1 and 0 (?)
predictions = predict(fit, newdata = test, type="response")
predictionsTrain = predict(fit, newdata = train, type="response")

yhat= predictions
yhatTrain= predictionsTrain
i=1
#loops to classify data
for(prob in predictions){
 
  if(prob>0.5){
    
    yhat[i]=1
  }else{
    yhat[i]=0
  }
  i=i+1
}
i=1
for(prob in predictionsTrain){
  
  if(prob>0.5){
    
    yhatTrain[i]=1
  }else{
    yhatTrain[i]=0
  }
  i=i+1
}

yhat2= predictions
yhatTrain2 = predictionsTrain
i=1
for(prob in predictions){
  
  if(prob>0.9){
    
    yhat2[i]=1
  }else{
    yhat2[i]=0
  }
  i=i+1
}
i=1
for(prob in predictionsTrain){
  
  if(prob>0.9){
    
    yhatTrain2[i]=1
  }else{
    yhatTrain2[i]=0
  }
  i=i+1
}
#creating a confusion matrix
confusionM=table(yhat, correctAnswers)
print(confusionM)

confusionM2=table(yhat2, correctAnswers)
print(confusionM2)

confusionMTrain=table(yhatTrain , correctAnswersTrain)

confusionMTrain2 = table(yhatTrain2, correctAnswersTrain)
print(confusionMTrain)
print(confusionMTrain2)


#getting the missclassification rate, missclassfunction made at the top of the code
missclassification = missclass(yhat, correctAnswers)
missclassification2 = missclass(yhat2, correctAnswers)

missclassificationTrain = missclass(yhatTrain, correctAnswersTrain)
missclassificationTrain2 = missclass(yhatTrain2, correctAnswersTrain)

print(paste0("missclass 1: ",missclassification))
print(paste0("missclass 2: ",missclassification2))
print(paste0("missclass train 1: ", missclassificationTrain))
print(paste0("missclass train 2: ", missclassificationTrain2))
#p(spam|x) >0,9 sets a higher boundry for how "spammy" a mail needs to be to be called spam


# kknn --------------------------------------------------------------------
#kknn does the modeling and the predictions at the same time. That is why both
# Training and test data i set as input in kknn, k sets the amount of nearest datapoints looked at
resultkknn = kknn(Spam~. , train=train, test=test, k=30)
resultkknnTrain = kknn(Spam~. , train=train, test=train, k=30)

#Extracting the fitted values from previous results

fitkknn = resultkknn$fitted.values
fitkknnTrain = resultkknnTrain$fitted.values

#classification you can use c() instead of "vector" below i believe
yhatkknn = vector(mode="numeric", length = 0)
for(prob in fitkknn){
  if(prob > 0.5){
    yhatkknn=c(yhatkknn,1)
    
  }else{
    yhatkknn=c(yhatkknn,0)
  }
}

yhatkknnTrain = vector(mode="numeric", length = 0)

for(prob in fitkknnTrain){
  if(prob > 0.5){
    yhatkknnTrain=c(yhatkknnTrain,1)
    
  }else{
    yhatkknnTrain=c(yhatkknnTrain,0)
  }
}

confusionMkknn = table(yhatkknn, correctAnswers)
confusionMkknnTrain= table(yhatkknnTrain, correctAnswersTrain)
print(confusionMkknn)
print(confusionMkknnTrain)

missclassificationkknn = missclass(yhatkknn, correctAnswers)
missclassificationkknnTrain = missclass(yhatkknnTrain, correctAnswersTrain)

print(missclassificationkknn)
print(missclassificationkknnTrain)


# k=1 in kknn -------------------------------------------------------------


resultkknn = kknn(Spam~. , train=train, test=test, k=1)
resultkknnTrain = kknn(Spam~. , train=train, test=train, k=1)

fitkknn = resultkknn$fitted.values
fitkknnTrain = resultkknnTrain$fitted.values

yhatkknn = vector(mode="numeric", length = 0)

for(prob in fitkknn){
  if(prob > 0.5){
    yhatkknn=c(yhatkknn,1)
    
  }else{
    yhatkknn=c(yhatkknn,0)
  }
}

yhatkknnTrain = vector(mode="numeric", length = 0)

for(prob in fitkknnTrain){
  if(prob > 0.5){
    yhatkknnTrain=c(yhatkknnTrain,1)
    
  }else{
    yhatkknnTrain=c(yhatkknnTrain,0)
  }
}

confusionMkknn = table(yhatkknn, correctAnswers)
confusionMkknnTrain= table(yhatkknnTrain, correctAnswersTrain)
print(confusionMkknn)
print(confusionMkknnTrain)

missclassificationkknn = missclass(yhatkknn, correctAnswers)
missclassificationkknnTrain = missclass(yhatkknnTrain, correctAnswersTrain)

print(missclassificationkknn)
print(missclassificationkknnTrain)


