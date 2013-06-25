#=====================================================================
# Script: Evaluate models by SSE with informativeness based ARCING 
#                                              last updated:2007.10.03
#                                                              by TSYo
#=====================================================================
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
attach(rawdata)
#----------------------------------------------------------------------
# Define parameters
dataset <- rawdata						# define dataset
#  Basic parameters
kfold   <- 200							# Number of fold in Cross-Validation
ntest   <- dim(dataset)[1]/kfold		# Size of testing data
ntrain  <- dim(dataset)[1]-ntest		# Size of training data
mods  	<- vector("list",length=kfold)	# List of models
preds 	<- vector("list",length=kfold)	# List of predictions
sse		<- array(dim=kfold)
#  Parameters for informativeness
infomts	<- array(data=0,dim=kfold)
infosco <- array(data=0,dim=dim(dataset)[1])
#  Parameters for arcing
miss.class <- rep(0,ntrain)				# Mis-classified data in training
predclass  <- rep(0,ntrain)				# Collection of predictions
prob.sel   <- rep((1/ntrain),ntrain)	# Selection probability
#
#----------------------------------------------------------------------
# Build distinction matrix for each record
#-----------------------------
# Split Cross-Validation groups
cvg <- cvidx(D=dataset, ngroup=kfold, rseed=12345)
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
# Clean up
detach(rawdata)
rm(kfold,dataset,ntrain,ntest,dtrain,dtest,i,mods,j,k,tspace,cvg,infomts,sse
   ,miss.class,prob.sel,train.mod,preds,predclass
  )