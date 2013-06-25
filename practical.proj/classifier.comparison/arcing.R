#======================================================================
# Script: Evalute various models by SSE with ARCING 
#                                              last updated:2006.06.26
#                                                              by TSYo
#======================================================================
#library(MASS)
library(nnet)
#library(e1071)
#library(class)
attach(raw.data)

# Define parameters
#
kboot <- 700
ntrain <- 200
ntest <- 200
dataset <- raw.data

mods <- vector("list",length=kboot)
preds <- vector("list",length=kboot)
miss.class <- rep(0,ntrain)
predclass <- rep(0,ntrain)
prob.sel <- rep((1/ntrain),ntrain)
#----------------------------------------------------------------------
#
# Start bootstrapping
set.seed(54321)

# Training 
for(i in 1:kboot){

# Create training data matrix
samp <- sample(ntrain, ntrain, replace=T, prob = prob.sel)
dtrain <- dataset[samp,]
dtest  <- dataset

#-----------------------------
# neural network
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
#-----------------------------

}

#-----------------------------
# Testing for whole data
#-----------------------------
predclass <- rep(0,ntrain)
for(i in 1:kboot){
  predclass <- predclass + preds[[i]]
}
predclass <- as.numeric((predclass/kboot)>0.5)
sse <- sum((dataset$y-predclass)^2)/ntrain
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
detach(raw.data)
rm(kboot,dataset,samp,ntrain,i,j,dtrain,dtest,ntest,
  miss.class,prob.sel,train.mod,preds,predclass
  )
