########################################################################                    
# Script to test PCA+kNN classifier with 10-fold Cross Validation
# T.S.Yo 2006.12.05
########################################################################                    
cv10pca <- function(xdata,ydata,kn=1,rseed=12345,...)
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
#----------------------------------------------------------------------
# Define parameters
  nvar <- dim(xdata)[2]
  ndat <- dim(xdata)[1]
  kfold <- 10
  sse <- array(dim=c(kfold,2),dimnames=list(NULL,c("k","PCA")))
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

    pred.knn <- pcaknn(dtrain, dtest, trclass, k=kn,...)

    sse[i,1] <- kn
    sse[i,2] <- sum(ifelse(ydata[samp[[i]]]==pred.knn$predict,0,1))/length(samp[[i]])
  }
  cat("Mean error for 10-fold CV:\n")
  print(colSums(sse)/kfold)
  invisible(sse)
}
