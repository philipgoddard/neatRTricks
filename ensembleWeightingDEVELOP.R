weightedRMSE <- function(modelWeights, trainPred, trainOutcome) {
  # This function returns the RMSE of the ensemble based upon
  # the weights for the predictions of each model.
  # Note that we take as input weights for number of models -1
  # as we can determine the last weight from all the others
  
  nPred <- length(modelWeights) + 1
    
  # check proportions in correct range
  # make so at the very least 2% each
  for(i in 1:(nPred - 1)) {
    if(modelWeights[i] < 0.02 | modelWeights[i] > 1) return(10^38)
  }
  
  # determine final weight from the others
  modelWeights <- c(modelWeights, 1 - sum(modelWeights))
  
  # check final weight
  if(modelWeights[nPred] < 0.02) return(10^38)
    
  weightedRmse <- vector('list', length = nPred)
  for(i in 1:nPred) {
    weightedRmse[i]  <- modelWeights[i] * sqrt(mean((trainPred[i] - trainOutcome)^2))                                   
  }
  Reduce(sum, weightedRmse) 
}

######################################################
######################################################

optimRMSE <- function(trainPred, trainOutcome) {
  
  # how many models did we use to make predictions?
  nPred <- ncol(trainPred)
  
  # randomly build up vector of n weights
  # which add to 1. Build up nStart of these
  nStart <- 50
  weights <- vector('list', length = nStart)
  
  for(i in 1:nStart) {
    draws <- as.list(-log(runif(nPred))) 
    denom <- Reduce(sum, draws) 
    weights[[i]] <- simplify2array(Map(function(x, y) x / y,
                                       draws, denom))
  }
  weights <- do.call(rbind.data.frame, weights)
  for (i in 1:ncol(weights)) names(weights)[i] <- i
  weights$RMSE <- NA
  
  # loop over starting values and optimise for
  # minimum RMSE for each of these
  for( i in 1:nStart) {
    optimWeights <- optim(weights[i, 1:nPred-1],
                          weightedRMSE,
                          method = "Nelder-Mead",
                          control = list(maxit = 5000),
                          trainPred = trainPred,
                          trainOutcome = trainOutcome)
    
    weights[i, 'RMSE'] <- optimWeights$value
    weights[i, 1:(nPred - 1)] <- optimWeights$par
    weights[i, nPred] <- 1 - sum(weights[i, 1:nPred - 1])
  }
  
  # output the set of weights that give the minimum RMSE
  # TODO perhaps output list containing weights, rmse?
  weights <- weights[order(weights$RMSE), ]
  as.vector(unlist(weights[1, 1:nPred]))
}

######################################################
######################################################

# TODO make a function to split predictions into quantiles
# then calculates weights for each quantile seperately.
# determine quantile by best initial model, then build upon that???
# decide best method for this.

# Make a final function for test set, optional either best
# weights, or best weights split into quantile

######################################################
######################################################

# make up some data
set.seed(123)
actual <- runif(20) * 100

for (i in 1:5) {
  tmp <- data.frame(actual + rnorm(20, 0, i))
  models <- if(i == 1) tmp else cbind(models, actual + rnorm(20, 0, i))
  names(models)[i] <- paste("model", i, sep = "")
}

# will output weights
blah <- optimRMSE(models, actual)
blah