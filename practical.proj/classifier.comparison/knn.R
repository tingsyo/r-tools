#======================================================================
# Script: Perform k-Nearest Neighbor with bootstrapping
#                                              last updated:2006.06.24
#                                                              by TSYo
#======================================================================
# Define parameters
#
kfold <- 100
dataset <- raw.data
sse <- array(dim = c(1,10), dimnames=list(NULL,1:10))
tmp <- array(dim = kfold)

#----------------------------------------------------------------------
#
# Start cross-validation
# Create bootstrapping training sample
set.seed(54321)
samp <- NULL
for(i in 1:kfold){
  samp <- c(samp,list(sample(200,200,replace=T)))
}

for(kn in 1:10) {
for(i in 1:kfold){

# Create training data matrix
dtrain <- dataset[samp[[i]],]
dtest  <- dataset[-samp[[i]],]

#-----------------------------
# k-nearest neighbor
#-----------------------------
#Training & predicting

  pred.knn <- knn(dtrain[,1:3], dtest[,1:3], dtrain$y, k=kn, prob=T)

#Computing SSE
  tmp[i] <- sum((dtest$y-as.numeric(pred.knn)+1)^2)/(dim(dtest)[1])
}
  
  sse[1,kn] <- mean(tmp)
}
print(sse)

# Clean up
rm(kfold,dataset,samp,i,dtrain,dtest,tmp,pred.knn,kn)
