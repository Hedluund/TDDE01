rm(list=ls())
library(ggplot2)

maxThetaFunc = function(theta,x){
  log_vector=c()
  maxdata=-Inf
  for(var in theta){
    logData=loglikehood(var, x)
    if(logData>maxdata){
      maxdata=logData
      bestTheta=var
    }
    log_vector = c(log_vector,logData)
  }
  print(paste0("max value: ", maxdata))
  print(paste0("Best Theta: ", bestTheta[1]))

  return(data.frame(besttheta=bestTheta,log_vector))
}

# this function was given in the task
loglikehood = function(theta, x){
  n=length(x)
  Xsum=sum(x)
  res = n*log(theta) - theta*Xsum
  return(res)
}

baysianloglikehood = function(theta, x, lamda){
  n=length(x)
  return(n*log(theta) - theta*sum(x)+log(lamda) - lamda*theta)
}

getbayesianlog = function(theta,x){
  log_vector=c()
  maxdata=-Inf
  for(var in theta){
    blog = baysianloglikehood(var, x, 10)
    if(blog>maxdata){
      maxdata = blog
      besttheta = var
    }
    log_vector = c(log_vector,blog)
  }
  print(paste0("max value: ", maxdata))
  print(paste0("Best Theta: ", besttheta[1]))
  return(data.frame(bestTheta=besttheta, blog_vector=log_vector))
}


colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))

data = read.csv2("machines.csv")
dataVector=data$Length
dataVectorFirstSix=data$Length[1:6]
qplot(dataVector, binwidth= 0.05)



t_vector=seq(0, 3, 0.0015)


dataf1=maxThetaFunc(t_vector,dataVector)
dataf2=maxThetaFunc(t_vector,dataVectorFirstSix)
dataf3=getbayesianlog(t_vector, dataVector)

plot(t_vector,dataf2$log_vector,col=(colfunc(1000)),ylim=c(-300,0))
lines(t_vector,dataf1$log_vector)
lines(t_vector,dataf3$blog_vector)

set.seed(12345)
generatedValues=rexp(50,1/0.912)

histGenerated=hist(generatedValues,breaks=10)
histDataObs=hist(dataVector,breaks=10)

plot(histGenerated,col="grey")
plot(histDataObs,add=T,col=rgb(0,0.5,0.5,0.5))

