# inputs will be list of model predictions (for training set), actual values
# use optim to minimise


weightEnsemble <- function(trainOutcome, inTrainPred) {
  # find quantiles of training data
  splits <- vector('list', length = 4)
  for (i in 1:4) splits[[i]] <- quantile(trainOutcome)[[i]]
  
  splits  
  
  # 1 use optim for weights

  
}


# x are the weights for the models (list)
weightedRMSE <- function(modelWeights, inTrainPred, trainOutcome) {
  # checks that weights are between 0 and 1
  
  nPred <- length(modelWeights) + 1
    
  # check proportions in correct range
  # make so at the very least 5% each
  for(i in 1:(nPred - 1)) {
    if(modelWeights[i] < 0.05 | modelWeights[i] > 1) return(10^38)
  }
  
  # determine final weight from the others
  modelWeights <- c(modelWeights, 1 - sum(modelWeights))
  
  # check final weight
  if(modelWeights[nPred] < 0.05 | modelWeights[nPred] > 1) return(10^38)
    
  weightedRmse <- vector('list', length = nPred)
  for(i in 1:nPred) {
    weightedRmse[i]  <- modelWeights[i] * sqrt(mean((inTrainPred[i] - trainOutcome)^2))                                   
  }

Reduce(sum, weightedRmse) 
}

weights <- c(0.1, 0.05, 0.5, 0.05)
weightedRMSE(weights, models, actual)

optimRMSE(models, prediction)

optimRMSE <- function(trainPred, trainOutcome) {
  
  # step 1- randomly build up vector of n-1 weights (add to 1)
  # build up 20 random starting weight vectors
  nPred <- 5 # ncol(trainPred)
  
  weights <- vector('list', length = 20)
  
  for(i in 1:20) {
    draws <- as.list(-log(runif(nPred))) 
    denom <- Reduce(sum, draws) 
    weights[[i]] <- simplify2array(Map(function(x, y) x / y,
                                       draws, denom))
  }
  # convert to data frame ()
  weights <- do.call(rbind.data.frame, weights)
  for (i in 1:ncol(weights)) names(weights)[i] <- i
  weights$RMSE <- NA
  
  
  # loop over starting values
  for( i in 1:20) {
    optimWeights <- optim(weights[i, 1:nPred-1],
                          weightedRMSE,
                          method = "Nelder-Mead",
                          control = list(maxit = 5000),
                          inTrainPred = models,
                          trainOutcome = actual)
    
    weights[i, 'RMSE'] <- optimWeights$value
    weights[i, 1:(nPred - 1)] <- optimWeights$par
    weights[i, nPred] <- 1 - sum(weights[i, 1:nPred - 1])
  }
  
  weights <- weights[order(weights$RMSE), ][1:3, ]
  weights[1, 1:nPred]
  
 
}











optimWeights <- optim(weights,
                      weightedRMSE,
                      method = "Nelder-Mead",
                      control = list(maxit = 5000),
                      inTrainPred = models,
                      trainOutcome = actual)

finalWeights <- NULL
finalWeights[1:4] <- optimWeights$par
finalWeights <- c(finalWeights, 1 - sum(finalWeights))






# start with few fitted models
# look at RMSE in quartiles as well as overall
# as using training data, can prescribe quartiles wrt to this
# for each quartile, want to minimise rmse using weighted average of 
# model predictions

# these weights will then be applied to test data when making final predictions
# from multiple models

# step 1: make up some data

actual <- runif(20) * 100

for (i in 1:5) {
  tmp <- data.frame(actual + rnorm(20, 0, i))
  models <- if(i==1) tmp else cbind(models, actual + rnorm(20, 0, i))
  names(models)[i] <- paste("model", i, sep = "")
}

weights <- replicate(4, 0.2)
inTrainPred <- models
trainOutcome <- actual
