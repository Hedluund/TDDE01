rm(list=ls())
library(ggplot2)
library(MASS)
library(COUNT)
library(tree)
set.seed(12345)

library(rpart)
# Naive bayes library
library(e1071)
data <- read.csv2("creditscoring.csv")

missclass=function(X,Y){
  size=length(X)
  rate=(1-sum(diag(table(X,Y)))/size)
  return (rate)
}
#Classificationfunction with the possibility to set prior
classif = function(x,class1,class2,prior){
  classificationVector = c()
  
  for(prob in x){
    if(prob>prior){
      classificationVector = c(classificationVector, class1)
    }else{
      classificationVector = c(classificationVector, class2)
    }
  }
  return(classificationVector)
}

# Task 1 and two below ----------------------------------------

#split data 0.5 0.25 0.25
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,] 



devianceFit = tree(good_bad~. , data=train, split="deviance")
giniFit = tree(good_bad~. , data = train, split= "gini")
# I think i set it to data frame to make it easier in the classif funktion
devianceYhatFit = data.frame(predict(devianceFit, newdata=test))
giniYhatFit = data.frame(predict(giniFit, newdata=test))

devianceYhatClassified =classif(devianceYhatFit$good,"good", "bad", 0.5 )
giniYhatClassified = classif(giniYhatFit$good, "good", "bad", 0.5)

devianceMissclass = missclass(devianceYhatClassified, test$good_bad)
giniMissclass = missclass(giniYhatClassified, test$good_bad)


plot(devianceFit)
text(devianceFit)

plot(giniFit)
text(giniFit)

summary(devianceFit)
summary(giniFit)

#Deviance has the best missclassification rate with 0.268 vs gini with 0.356

## task 3 below ---------------------------------------------------------

trainScore=seq(0,15)
testScore=rep(0,15)

bestScore=999999
# uses 2:15 because a tree can´t have less than 2 terminal nodes
for(i in 2:15) {
  
  prunedTree=prune.tree(devianceFit,best=i)
  pred=predict(prunedTree, newdata=valid,
               type="tree")
  trainScore[i]=deviance(prunedTree)/2
  testScore[i]=deviance(pred)
  if(testScore[i]<bestScore){
    bestTree = prunedTree
    bestScore = testScore[i]
    bestPredict = data.frame(predict(prunedTree, newdata =valid, type="class"))
    bestPredictRaw = data.frame(predict(prunedTree, newdata =test, type="vector"))
  }
}

plot(2:15, trainScore[2:15], type="b", col="red", ylim = c(220,400))
points(2:15, testScore[2:15], type="b", col="blue")

# add one to index below to adjust for the array starting on 2
# help('%in%'): match returns a vector of the positions of (first) matches of its first argument in its second.
bestScore = testScore[which.min(testScore[2:15])+1] 
amountOfLeaves = which(testScore %in% bestScore)


plot(bestTree)
text(bestTree)
## depth 3 variables:history, duration, savings

## esitmate missclassification error for test data

missclassEstimation = missclass(bestPredict$predict.prunedTree..newdata...valid..type....class..,valid$good_bad)

#estimated error 0.264, Has the same distribution as valid.

#task 4 below ----------------------------------------------
naiveFit = naiveBayes(good_bad~., data=train)

naiveYhatFittest = predict(naiveFit, newdata=test ,type="class")

naiveYhatFitTrain = predict(naiveFit, newdata=train ,type="class")

missClassNaiveTest = missclass(naiveYhatFittest, test$good_bad)

missClassNaiveTrain = missclass(naiveYhatFitTrain, train$good_bad)

table(naiveYhatFittest, test$good_bad)
table(naiveYhatFitTrain, train$good_bad)

##missclass rate train = 0.3 , test = 0.316

#task 5 below ----------------------------------------------

#Uses type = "raw" to get the probabilities instead of class
naiveYhatFit = predict(naiveFit, newdata=test ,type="raw")

pi = seq(0.05,0.95,0.05)

#TPR = true positive rate FPR = false positive rate

tprNaive = c()
fprNaive = c()

tprTree = c()
fprTree = c()

naiveFit = naiveBayes(good_bad~., data=train)

naiveYhatFit = predict(naiveFit, newdata=test ,type="raw")

for(prob in pi){
  
  claNaive = classif(naiveYhatFit[,2], "good", "bad", prob)
  claTree = classif(bestPredictRaw$good, "good", "bad", prob)
  
  valTableNaive=table(test$good_bad, claNaive)
  valTableTree = table(test$good_bad, claTree )
  #Important to check where in the table you want to extract your numbers
  amountBad = sum(valTableNaive[1,])
  amountGood = sum(valTableNaive[2,])
  
  # Nedan kollar vi om det blir så att en column inte får några värde för att alla
  # ses som negative/positive och då lägger vi till en 0 0 column för att det ska bli rätt
  if(ncol(valTableNaive)<2){
   
    if(colnames(valTableNaive)[1] == "bad"){
      valTableNaive =cbind(valTableNaive,"good" = c(0,0))
    }else {
      valTableNaive =cbind("bad" = c(0,0), valTableNaive)
    }
  }
  if(ncol(valTableTree)<2){
    
    if(colnames(valTableTree)[1] == "bad"){
      valTableTree=cbind(valTableTree,"good" = c(0,0))
    }else {
      valTableTree=cbind("bad" = c(0,0), valTableTree)
    }
  }
  tprN = valTableNaive[2,2]/amountGood
  fprN = valTableNaive[1,2]/amountBad
  
  tprT = valTableTree[2,2]/amountGood
  fprT = valTableTree[1,2]/amountBad
  
  tprNaive = c(tprNaive, tprN)
  fprNaive = c(fprNaive, fprN)
  
  tprTree = c(tprTree, tprT)
  fprTree = c(fprTree, fprT)
}

fprNaive= c(fprNaive[1:19])

plot(fprTree, tprTree,xlim = c(0,1), ylim = c(0,1),type="l")
points(fprNaive, tprNaive, col="red", type="l")

#task 6 below ---------------------------------------------

naiveFitLoss = naiveBayes(good_bad~., data=train)

naiveYhatFittestLoss = classif(predict(naiveFitLoss, newdata=test ,type="raw")[,2],"good","bad", 10/11)

naiveYhatFitTrainLoss = classif(predict(naiveFitLoss, newdata=train ,type="raw")[,2],"good","bad", 10/11)



missClassNaiveTestLoss = missclass(naiveYhatFittestLoss, test$good_bad)

missClassNaiveTrainLoss = missclass(naiveYhatFitTrainLoss, train$good_bad)

table(naiveYhatFittestLoss, test$good_bad)
table(naiveYhatFitTrainLoss, train$good_bad)



