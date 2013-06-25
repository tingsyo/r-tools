########################################################################                    
# Script to test NCA-kNN classifier with 10-fold Cross Validation
# T.S.Yo 2006.10.29
########################################################################                    
cv10nca <- function(xdata,ydata,rseed=12345,...)
{
#----------------------------------------------------------------------
# Check input
if(dim(xdata)[1]!=length(ydata))
{
  cat("\nNumber of data and class labels do not match, abort.\n")
  return(NULL)
}
#----------------------------------------------------------------------
# Load library for kNN
  library(class)          
#----------------------------------------------------------------------
# Define parameters
  nvar <- dim(xdata)[2]
  ndat <- dim(xdata)[1]
  kfold <- 10
  sse <- array(dim=c(kfold,3),dimnames=list(NULL,c("k","kNN","NCA")))
#----------------------------------------------------------------------
# Create 10 folds testing sample
  set.seed(rseed)
  ordering <- sample(ndat,ndat)
  samp <- NULL
  npertest <- (ndat%/%kfold)
  nexttest <- (ndat%%kfold)
  iflag <- 1
  # Pick up sample to each fold in turn
  for(i in 1:kfold){
    samp <- c(samp,list(ordering[iflag:(iflag+npertest-1)]))
    iflag <- iflag+npertest
  }
  # Pick up one additional sample for first "nexttest" folds
  if(nexttest!=0){
  for(i in 1:nexttest){
    samp[[i]] <- c(samp[[i]],ordering[iflag])
    iflag <- iflag+1
  }}
#
#----------------------------------------------------------------------
  # Start 10-fold cross-validation
  for(i in 1:kfold){

    # Create training/testing data matrix
    dtrain <- xdata[-samp[[i]],]
    trclass <- ydata[-samp[[i]]]
    dtest  <- xdata[samp[[i]],]

    pred.nca <- ncaknn(dtrain, dtest, trclass,...)
    kn <- pred.nca$k
    pred.knn <- knn(dtrain, dtest, trclass, k=kn)

    sse[i,1] <- kn
    sse[i,2] <- sum(ifelse(ydata[samp[[i]]]==pred.knn,0,1))/length(samp[[i]])
    sse[i,3] <- sum(ifelse(ydata[samp[[i]]]==pred.nca$predict,0,1))/length(samp[[i]])
  }
  cat("Mean error for 10-fold CV:\n")
  print(colSums(sse)/kfold)
  invisible(sse)
}
