#=====================================================================
# Script: Evaluate models by SSE with informativeness based ARCING 
#                                              last updated:2007.10.03
#                                                              by TSYo
#=====================================================================

#library(MASS)
library(nnet)
#library(e1071)
#library(class)

#source("info.R")
#----------------------------------------------------------------------
attach(rawdata)

# Define parameters
#  Basic parameters
kboot   <- 20							# Number of bootstraping
ntrain  <- 200							# size of training data
ntest   <- 200							# size of testing data
dataset <- rawdata						# define dataset
mods  	<- vector("list",length=kboot)	# list of models
preds 	<- vector("list",length=kboot)	# list of predictions
#  Parameters for arcing
miss.class <- rep(0,ntrain)				# Mis-classified data in training
predclass  <- rep(0,ntrain)				# Collection of predictions
prob.sel   <- rep((1/ntrain),ntrain)	# Selection probability
#prob.sel   <- infosco					# Selection probability based on informativeness
#----------------------------------------------------------------------
# Start bootstrapping
set.seed(12345)

# Training 
for(i in 1:kboot){
	# Create training data matrix
	samp <- sample(ntrain, ntrain, replace=T, prob = prob.sel)
	dtrain <- dataset[samp,]
	dtest  <- dataset
#-----------------------------
# Training and testing neural network
#-----------------------------
	train.mod <- nnet(y~., data=dtrain, skip=T, entropy=T, size=4, decay=0.01, maxit=1000, trace=0)
	mods[[i]] <- train.mod
	preds[[i]] <- as.numeric(predict(mods[[i]], dataset[,1:3], type="class"))
#-----------------------------
# Updating selection probability
#-----------------------------
	miss.class <- miss.class + abs(dataset$y-preds[[i]])
	prob.sel <- 1 + miss.class^4
	prob.sel <- prob.sel/sum(prob.sel)
	#prob.sel <- (prob.sel/sum(prob.sel)+infosco)/2
}
#-----------------------------

#-----------------------------
# Testing for whole data
#-----------------------------
predclass <- rep(0,ntrain)						# Clear up the prediction
for(i in 1:kboot){								# Predict with each trained model
	predclass <- predclass + preds[[i]]
}
predclass <- as.numeric((predclass/kboot)>0.5)	# Vote for final prediction
sse <- sum((dataset$y-predclass)^2)/ntest		# SSE
print(sse)
#-----------------------------

#-----------------------------
# Testing by re-sampling
#-----------------------------
set.seed(654321)
# Predicting 
sse <- array(dim = 10)

for(j in 1:10){
  preds <- vector("list",length=kboot)
  predclass <- rep(0,ntest)
  dtest <- dataset[sample(ntrain,ntest,replace=T),]

  for(i in 1:kboot){
    preds[[i]] <- as.numeric(predict(mods[[i]], dtest[,1:3], type="class"))
    predclass <- predclass + preds[[i]]
  }
  predclass <- as.numeric((predclass/kboot)>0.5)
  sse[j] <- sum((dtest$y-predclass)^2)/ntest
}
print("Error rate in 10 resamples")
print(c("Mean: ",mean(sse)))
print(c("Sdev: ",sd(sse)))
#-----------------------------

# Clean up
detach(rawdata)
rm(kboot,dataset,samp,ntrain,i,j,dtrain,dtest,ntest,
   miss.class,prob.sel,train.mod,preds,predclass
  )