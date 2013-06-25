arcx4 <- function(dataset,nmod=500,rseed=54321){
#======================================================================
# Function: arcx4nnet
#                                              last updated:2006.06.28
#                                                              by TSYo
#----------------------------------------------------------------------
# Usage:
#     arcx4.nnet(dataset)
#----------------------------------------------------------------------
# Description:
#     Learn a k-model-ensemble for classification
#----------------------------------------------------------------------
# Input arguments:
#     D
#         A data matrix containing the data and class label.  Each row
#         contains a record.
#     nmod
#         An integer indicate the number of groups to be divided into.  
#----------------------------------------------------------------------
# Return objects:
#     mods
#         A list of model that .
#----------------------------------------------------------------------
# Examples:
#     
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
    samp <- sample(ntrain, ntrain, replace=T, prob = prob.sel)
    dtrain <- dataset[samp,]
    dtest  <- dataset
#-----------------------------
# neural network
#-----------------------------
#    mods[[i]] <- nnet(y~x.1+I(x.2^2), data=dtrain, skip=T, entropy=T, size=4, decay=0.01, maxit=1000, trace=0)
#    preds[[i]] <- as.numeric(predict(mods[[i]], dataset[,1:3], type="class"))

#-----------------------------
# logistic regression
#-----------------------------
#    mods[[i]] <- glm(y~x.1+I(x.2^2),data=dtrain,family=binomial)
#    preds[[i]] <- as.numeric(predict(mods[[i]],newdata=dataset[,1:3],type="response") > 0.5)
    
#-----------------------------
# discriminant analysis
#-----------------------------
    mods[[i]] <- lda(y~.,data=dtrain)
    preds[[i]] <- as.numeric(predict(mods[[i]],dataset[,1:3])$class)-1
    
#-----------------------------
# Updating selection probability
#-----------------------------
    miss.class <- miss.class + abs(dataset$y-preds[[i]])
    prob.sel <- 1 + miss.class^4
    prob.sel <- prob.sel/sum(prob.sel)
  }
#----------------------------------------------------------------------
  return(mods)
}