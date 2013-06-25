pred.class <- function(x, dtrain=raw.data, nmod=100, rseed=54321){
#======================================================================
# Function: pred.class
#                                              last updated:2006.06.28
#                                                              by TSYo
#----------------------------------------------------------------------
# Usage:
#     pred.class(predictors)
#----------------------------------------------------------------------
# Description:
#     Perform prediction from the trained classifier and given predictors.
#----------------------------------------------------------------------
# Input arguments:
#     D
#         A data matrix containing the data to be divided.  Each row
#         contains a record.
#     ngroup
#         An integer indicate the number of groups to be divided into.  
#----------------------------------------------------------------------
# Return objects:
#     class
#         A vector of predicted class labels from the given predictor x.
#----------------------------------------------------------------------
# Examples:
#     
#======================================================================
# Checking arguments
    if(missing(x))
        stop("Please specify a matrix of 3 columns which containing the predictors.")
    if(dim(x)[2] != 3)
        stop("Please specify a matrix of 3 columns which containing the predictors.")
    if(missing(nmod))
        print("Use bagging with 100 models by default.")
#    
#----------------------------------------------------------------------
# Loading libraries
    library(MASS)
#    library(class)
#    library(nnet)
#----------------------------------------------------------------------
# Setting Parameters
    ntest <- dim(x)[1]          # Number of test cases
    preds <- vector("list", length=nmod)
    predclass <- rep(0,ntest)
# Training
    mods <- bagging(dtrain,nmod,rseed)
    #mods <- arcx4(dtrain,nmod)
# Predicting
    for(i in 1:nmod){
        #-----------------------------
        # N-Net
        #preds[[i]] <- as.numeric(predict(mods[[i]], x, type="class"))
        #-----------------------------
        # Logistic regression
        #preds[[i]] <- as.numeric(predict(mods[[i]],newdata=x,type="response") > 0.5)
        #-----------------------------
        # Discriminant analysis
        preds[[i]] <- as.numeric(predict(mods[[i]],x)$class)-1
        #-----------------------------

        predclass <- predclass + preds[[i]]
    }
    predclass <- as.numeric((predclass/nmod)>0.5)

  return(predclass)
}