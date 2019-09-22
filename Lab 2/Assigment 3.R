rm(list=ls())
set.seed(12345)
library(ggplot2)
library(tree)
library(boot)
data = read.csv2("State.csv")

#order the data frame on the column "MET"
newData = data[order(data$MET),]

plot(newData$MET, newData$EX)

# Believe a regression would be appropriate since it looks like it fits an X^2 somewhat good
# Mincut sets minimum numbers of observations in node
treeModel = tree(EX~MET, data = newData, control = tree.control(nobs = nrow(newData),mincut = 8 ))
plot(treeModel)
text(treeModel, pretty = 2)
cvTreeModel = cv.tree(treeModel)
plot(cvTreeModel$dev)

bestSize = cvTreeModel$size[which.min(cvTreeModel$dev)]
#prune tree to best size given in cv.tree and extracted on the row above
bestTree= prune.tree(treeModel, best = bestSize)
plot(bestTree)
text(bestTree, pretty = 2)

predictions = predict(bestTree, data=newData)
#Important to plot the newData$MET with the predictions to get a proper x-axis scale
plot(newData$MET,newData$EX, col = "darkgreen")
points(newData$MET, predictions,col="blue")
#Breaks sets how many staples you should have
hist(residuals(bestTree), breaks = 7)
set.seed(12345)

f=function(data, ind){
  data1=data[ind,]# extract bootstrap sample
  res=tree(EX~MET, data=data1) #fit tree model
  #predict values for all Area values from the original data
  priceP=predict(res,newdata=newData)
  return(priceP)
}
res=boot(newData, f, R=1000) #make bootstrap

confBands = envelope(res)
plot(newData$MET,newData$EX, col = "magenta", pch=16)
points(newData$MET,confBands$point[2,], type="l", col="black")
points(newData$MET,confBands$point[1,], type="l", col="black")


#The plot shows that the expenditures are bigger for people living in the country or in big cities.

