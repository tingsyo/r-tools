cvinfo <- function(D=NULL,ncv=10,rseed=0){
#======================================================================
# Function: cvinfo
#                                              last updated:2007.10.03
#                                                              by TSYo
#----------------------------------------------------------------------
# Usage:
#     cvinfo(D, ncv, rseed)
#----------------------------------------------------------------------
# Description:
#   Compute informativeness via n-fold cross validation
#----------------------------------------------------------------------
# Input arguments:
#     D
#         A data matrix containing the data to be divided.  Each row
#         contains a record.
#     ncv
#         An integer indicate the number of groups to be divided into.  
#     rseed
#         An integer serves as the random seed for sampling.  
#----------------------------------------------------------------------
# Return objects:
#     data.info
#         An array of informativeness .
#----------------------------------------------------------------------
# Examples:
#     
#======================================================================
# How to compute informativeness?
# 1. Perform a n-fold cross-validation for a certain classifier,
#    keep the SSE and StdDev of SSE.
# 2. For each record, it is used for training (n-1) models.  Hence we
#    may derive a distinction matrix, where each element d(i,j) is 
#    defined as ABS(SSE(M_i)-SSE(M_j)). 
#    (Or use StdDev for significance, and returns 0/1)
# 3. Sum up the distinction matrix as the informativeness.
#----------------------------------------------------------------------
source("cv.R")
library(nnet)
#----------------------------------------------------------------------
# Define parameters
dataset <- D							# define dataset
#  Basic parameters
kfold   <- ncv							# Number of fold in Cross-Validation
ntest   <- dim(dataset)[1]/kfold		# Size of testing data
ntrain  <- dim(dataset)[1]-ntest		# Size of training data
mods  	<- vector("list",length=kfold)	# List of models
preds 	<- vector("list",length=kfold)	# List of predictions
sse		<- array(dim=kfold)
#  Parameters for informativeness
infomts	<- array(data=0,dim=kfold)
infosco <- array(data=0,dim=dim(dataset)[1])
#
#----------------------------------------------------------------------
# Build distinction matrix for each record
#-----------------------------
# Split Cross-Validation groups
cvg <- cvidx(D=dataset, ngroup=kfold, rseed=rseed)
#-----------------------------
# Start Cross-Validation
#-----------------------------
for(i in 1:kfold){
	dtrain <- dataset[cvg$cv.train[[i]],]
	dtest  <- dataset[cvg$cv.test[[i]],]
#	print(dtest)
#-----------------------------
# Training and testing neural network
#-----------------------------
	train.mod <- nnet(y~., data=dtrain, skip=T, entropy=T, size=4, decay=0.01, maxit=1000, trace=0)
	preds[[i]]<- as.numeric(predict(train.mod, dtest[,1:3], type="class"))
	sse[i] <- sum((dtest$y-preds[[i]])^2)/ntest
}
#-----------------------------
# Distinction matrix
#-----------------------------
for(i in 1:kfold){
	infomts[i]=0
	tspace <- seq(1:kfold)
	tspace <- tspace[-i]
	for (j in 1:(kfold-2)){
	for (k in (j+1):(kfold-1)){
		infomts[i]=infomts[i]+abs(sse[tspace[j]]-sse[tspace[k]])
	}
	}
	infosco[cvg$cv.test[[i]]]=infomts[i]
}
infosco=infosco/sum(infosco)
#----------------------------------------------------------------------
return(infosco)
}