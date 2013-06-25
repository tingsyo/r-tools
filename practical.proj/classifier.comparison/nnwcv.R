#======================================================================
# Script: Perform Neural Network with cross validation
#                                              last updated:2006.06.24
#                                                              by TSYo
#======================================================================
# Define parameters
#
kfold <- 200
dataset <- raw.data
sse <- array(dim = c(3,5), dimnames=list(c(0.0001,0.001,0.01),c(2,4,6,8,10)))
tmp <- array(dim = kfold)

#----------------------------------------------------------------------
#
# Start cross-validation
cvg <- cvidx(dataset,kfold,12345)

#for(j in 1:3){                  # loop for lambda
#for(k in 1:5){                  # loop for number of hidden nodes
j<-1
k<-1
#Set parameters
  lambda <- 0 + 10^(j-1)*0.0001
  nhidden <- 2*k
#  print(c(lambda,nhidden))
for(i in 1:kfold){

# Create training data matrix
dtrain <- dataset[cvg$cv.train[[i]],]
dtest  <- dataset[cvg$cv.test[[i]],]
#-----------------------------
# neural network
#-----------------------------
#Set parameters
  set.seed(12345)
  yy <- class.ind(dtrain$y)
#Training
  train.nnet <- nnet(y~x.1+I(x.2^2), data=raw.data, skip=T, entropy=T,size=4, decay=0.01, maxit=1000,trace=0)

#Predicting
  pred.nnet <- predict(train.nnet, dtest[,1:3], type="class")

#Computing SSE
  tmp[i] <- sum((dtest$y-as.numeric(pred.nnet))^2)/(dim(dtest)[1])
}
  
  sse[j,k] <- mean(tmp)
#}
#}
print(sse)

# Clean up
rm(kfold,dataset,cvg,i,dtrain,dtest,tmp,
  j,k,lambda,nhidden,yy,train.nnet,pred.nnet
  )
