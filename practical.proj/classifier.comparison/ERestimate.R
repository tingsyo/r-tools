#======================================================================
# Script: Evalute ARC-X4 model ensemble by cross-validation
#                                              last updated:2006.06.28
#                                                              by TSYo
#======================================================================
#library(MASS)
#library(nnet)
#library(e1071)
#library(class)
#attach(raw.data)

source("arcx4.R")
source("bagging.R")
source("pred.class.R")

# Define parameters
#
nfold <- 200
nmod <- 50
dataset <- raw.data
sse <- array(dim = nfold)

#======================================================================
# Start cross-validation
cvg <- cvidx(dataset,nfold,12345)

for(i in 1:nfold){
# Create training data matrix
  dtrain <- dataset[cvg$cv.train[[i]],]
  dtest  <- dataset[cvg$cv.test[[i]],]
  ntest <- dim(dtest)[1]
#-----------------------------
# Testing by test case
#-----------------------------
  predc <- pred.class(dtest[,-4], dtrain, nmod)
  sse[i] <- sum((dtest$y-predc)^2)/ntest
}
print(c("Mean: ",mean(sse)))
print(c("Sdev: ",sd(sse)))
#-----------------------------

# Clean up
#detach(raw.data)
rm(nfold,nmod,dataset,cvg)
rm(i,dtrain,dtest,ntest,predc)
