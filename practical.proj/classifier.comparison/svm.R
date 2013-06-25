#======================================================================
# Script: Perform SVM with cross validation
#                                              last updated:2006.06.24
#                                                              by TSYo
#======================================================================
# Define parameters
#
kfold <- 200
dataset <- raw.data
sse <- array(dim = c(1,4), dimnames = list(NULL,c("Linear","Polynomial","Radial","Sigmoid")))
tmp <- array(dim = c(kfold,4))

#----------------------------------------------------------------------
#
# Start cross-validation
cvg <- cvidx(dataset,kfold,12345)

for(i in 1:kfold){

# Create training data matrix
dtrain <- dataset[cvg$cv.train[[i]],]
dtest  <- dataset[cvg$cv.test[[i]],]
#-----------------------------
# SVM kernel : linear
#-----------------------------
  raw.svm <- svm(dtrain[,1:3], as.character(dtrain$y), type='C', kernel='linear')
  pred.svm <- (as.numeric(predict(raw.svm,dtest[,1:3]))-1)
  tmp[i,1] <- sum((dtest$y-pred.svm)^2)/(dim(dtest)[1])
  
#-----------------------------
# SVM kernel : polynomial
#-----------------------------
  raw.svm <- svm(dtrain[,1:3], as.character(dtrain$y), type='C', kernel='polynomial')
  pred.svm <- (as.numeric(predict(raw.svm,dtest[,1:3]))-1)
  tmp[i,2] <- sum((dtest$y-pred.svm)^2)/(dim(dtest)[1])

#-----------------------------
# SVM kernel : radial basis
#-----------------------------
  raw.svm <- svm(dtrain[,1:3], as.character(dtrain$y), type='C', kernel='radial')
  pred.svm <- (as.numeric(predict(raw.svm,dtest[,1:3]))-1)
  tmp[i,3] <- sum((dtest$y-pred.svm)^2)/(dim(dtest)[1])

#-----------------------------
# SVM kernel : sigmoid
#-----------------------------
  raw.svm <- svm(dtrain[,1:3], as.character(dtrain$y), type='C', kernel='sigmoid')
  pred.svm <- (as.numeric(predict(raw.svm,dtest[,1:3]))-1)
  tmp[i,4] <- sum((dtest$y-pred.svm)^2)/(dim(dtest)[1])

}

for(i in 1:4){  
  sse[1,i] <- mean(tmp[,i])
}

# Clean up
rm(kfold,dataset,cvg,i,dtrain,dtest,tmp,raw.svm,pred.svm)
