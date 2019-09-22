library(neuralnet)
set.seed(1234567890)
rm(list=ls())

Var <- runif(50, 0, 10)
# Generating the sin function
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
val <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1]
sseVector = c()

# 31 because we have 10 in the hidden layer. and 1 depth so every node needs one bias(10) and one
# coeff(10. Then the end node needs 1 for every connected node(10) and one bias
winit <- runif(31, -1, 1)

bestSSE = 1000
  for(i in 1:10) {
    nn <- neuralnet(Sin~Var , data=tr, startweights = winit, hidden=10, threshold = (i/1000))
    fit <- compute(nn, val$Var)
    # Can also use mse , gives same resull
    sse <- sum((fit$net.result -val$Sin)^2)
    sseVector = c(sseVector, sse)
    if(sse < bestSSE){
      bestNN <- nn
      bestSSE <- sse
      bestFit <- fit
    }
  }
#order = data.frame(cbind(FPRs_tree, TPRs_tree))
newval = val[order(Var),]
#with(order, lines(FPRs_tree, TPRs_tree, pch=16, col="blue"))
plot(sseVector, type="o")
plot(val$Var ,bestFit$net.result)

points(newval$Var, newval$Sin, col = "red")

#optimal thresshold 4/1000

