########################################################################                    
# Script to test NCA-kNN classifier with LOO
# T.S.Yo 2006.10.23
########################################################################                    
cvloonca <- function(xdata,ydata,...)
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
#  source("nca.r")
#  source("ncaknn.r")
#----------------------------------------------------------------------
# Define parameters
  nvar <- dim(xdata)[2]
  ndat <- dim(xdata)[1]
  kfold <- ndat
  sse <- array(dim=c(kfold,2),dimnames=list(NULL,c("1NN","NCA")))
#----------------------------------------------------------------------
# Create testing sample
  samp <- seq(1:ndat)
#----------------------------------------------------------------------
  # Start LOO cross-validation
  for(i in 1:kfold){

    # Create training/testing data matrix
    dtrain <- xdata[-samp[i],]
    trclass <- ydata[-samp[i]]
    dtest  <- xdata[samp[i],]

    pred.nca <- ncaknn(dtrain, dtest, trclass,...)
    kn <- pred.nca$k
    pred.knn <- knn(dtrain, dtest, trclass, k=kn)

    sse[i,1] <- ifelse(ydata[samp[i]]==pred.knn,0,1)
    sse[i,2] <- ifelse(ydata[samp[i]]==pred.nca$predict,0,1)
  }
  cat("Leave-One-Out error:\n")
  print(colSums(sse)/kfold)
  invisible(sse)
}
