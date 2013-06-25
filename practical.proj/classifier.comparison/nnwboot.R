#======================================================================
# Script: Perform Neural Network with bootstrapping
#                                              last updated:2006.06.24
#                                                              by TSYo
#======================================================================
library(nnet)

# Define parameters
#

kboot <- 100
dataset <- raw.data
nraw <- dim(dataset)[1]

sse <- array(dim = c(3,5), dimnames=list(c(0.0001,0.001,0.01),c(2,4,6,8,10)))
tmp <- array(dim = kboot)

#----------------------------------------------------------------------
# Create bootstrapping training sample
set.seed(54321)
samp <- NULL
for(i in 1:kboot){
  samp <- c(samp,list(sample(nraw,nraw,replace=T)))
}

# Start bootstrapping

for(j in 1:3){                  # loop for lambda
for(k in 1:5){                  # loop for number of hidden nodes
#Set parameters
  lambda <- 0 + 10^(j-1)*0.0001
  nhidden <- 2*k
  print(c(lambda,nhidden))
for(i in 1:kboot){
#print(i)
# Create training data matrix
dtrain <- dataset[samp[[i]],]
dtest  <- dataset[-samp[[i]],]
ntest <- dim(dtest)[1]
if(ntest !=0){
#-----------------------------
# neural network
#-----------------------------
#Set parameters
  set.seed(12345)
  yy <- class.ind(dtrain$y)
#Training
  train.nnet <- nnet(dtrain[,-4], yy, skip=T, softmax=T, size=nhidden, decay=lambda, maxit=1000, trace=0)
#  train.nnet <- nnet(y~x.1+I(x.2^2), data=dtrain, skip=T, entropy=T, size=nhidden, decay=lambda, maxit=1000,trace=0)
  
#Predicting
  pred.nnet <- predict(train.nnet, dtest[,-4], type="class")

#Computing SSE
  tmp[i] <- sum((dtest$y-as.numeric(pred.nnet))^2)/(dim(dtest)[1])
} # end if
} # end bootstrapping
  sse[j,k] <- mean(tmp)
}
} # loop through all parameters

print(sse)

# Clean up
rm(kboot,dataset,nraw,samp,ntest,dtrain,dtest,tmp,
  i,j,k,lambda,nhidden,yy,train.nnet,pred.nnet
  )
