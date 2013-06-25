bagging <- function(dataset,nmod=100,rseed=54321){
#======================================================================
# Script: Evalute various models by SSE with bootstrap aggregating 
#                                              last updated:2006.06.28
#                                                              by TSYo
#======================================================================
# Checking arguments
    if(missing(dataset))
        stop("Please specify a matrix for training data.")
    if(missing(nmod))
        print("Use arcing-nnet with 500 runs by default.")
#    
#----------------------------------------------------------------------
library(nnet)

#-----------------------------
# Define parameters
#-----------------------------
  kboot <- nmod
  ntrain <- dim(dataset)[1]

  mods <- vector("list",length=kboot)
  preds <- vector("list",length=kboot)
  miss.class <- rep(0,ntrain)
  predclass <- rep(0,ntrain)
  prob.sel <- rep((1/ntrain),ntrain)
#----------------------------------------------------------------------
# Start bootstrapping
  set.seed(rseed)

# Training 
  for(i in 1:kboot){
#-----------------------------
# Create training data matrix
#-----------------------------
    samp <- sample(ntrain, ntrain, replace=T)
    dtrain <- dataset[samp,]
    dtest  <- dataset
#-----------------------------
# neural network
#    mods[[i]] <- nnet(y~x.1+I(x.2^2), data=dtrain, skip=T, entropy=T, size=4, decay=0.01, maxit=1000, trace=0)
#-----------------------------
# logistic regression
#    mods[[i]] <- glm(y~x.1+I(x.2^2), data=dtrain,family=binomial)
#-----------------------------
# discriminant analysis
    mods[[i]] <- qda(y~.,data=dtrain)
#-----------------------------
  }
# End bootstrapping
#----------------------------------------------------------------------
  return(mods)
}
#

