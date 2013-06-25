cvidx <- function(D=NULL,ngroup=2,rseed=54321){
#======================================================================
# Function: cvidx
#                                              last updated:2006.06.22
#                                                              by TSYo
#----------------------------------------------------------------------
# Usage:
#     cvidx(D, ngroup)
#----------------------------------------------------------------------
# Description:
#   Create an index matrix for k-fold cross-validation
#----------------------------------------------------------------------
# Input arguments:
#     D
#         A data matrix containing the data to be divided.  Each row
#         contains a record.
#     ngroup
#         An integer indicate the number of groups to be divided into.  
#----------------------------------------------------------------------
# Return objects:
#     cv.train
#         A list with length-ngroup of arrays containing indices of training data.
#     cv.test
#         A list with length-ngroup of arrays containing indices of testing data.
#----------------------------------------------------------------------
# Examples:
#     
#======================================================================
# Checking arguments
    if(missing(D))
        stop("Please specify the matrix containing data to be divided.")
    if(missing(ngroup))
        print("Use ngroup = 2 by default.")
    if(missing(rseed))
        print("Use seed = 54321 by default.")
#    
#----------------------------------------------------------------------
# Set random seed, for replicability
set.seed(rseed)
#----------------------------------------------------------------------
#
# Define dataset, N, k, (if k = N, it performs leave-one-out CV)
dataset <- D
N <- dim(dataset)[1]

# number of folds 
if(ngroup >= N){
  print("ngroup > sample size, use leave-one-out cv instead.")
  k <- N
} else {
  k <- ngroup                     
}

# Define fold-size
foldSize <- rep((N%/%k),k)      # fold-size = N/k
if(N%%k!=0){                    # for residuals, add one to first (N%/%k) groups
  for(i in 1:(N%%(N%/%k))){
    foldSize[i] <- (N%/%k)+1    
  }
}

#----------------------------------------------------------------------
#
# Re-ordering the data indices
indlist <- sample(N,N)
      

cvtest  <- list(indlist[1:foldSize[1]])
cvtrain <- list(indlist[-(1:foldSize[1])])

for(j in 2:k){
  counter1 <- sum(foldSize[1:(j-1)]) + 1
  counter2 <- sum(foldSize[1:j])
  cvtest <- c(cvtest,list(indlist[counter1:counter2]))
  cvtrain <- c(cvtrain,list(indlist[-(counter1:counter2)]))
}


#----------------------------------------------------------------------
#
    return(list(cv.train=cvtrain,cv.test=cvtest))
}
