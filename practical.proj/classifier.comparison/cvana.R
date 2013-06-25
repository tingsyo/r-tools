#======================================================================
# Script: Evalute various models by SSE with crross-validation
#                                              last updated:2006.06.25
#                                                              by TSYo
#======================================================================
library(MASS)
library(nnet)
#library(e1071)
library(class)
# Define parameters
#
kfold <- 10
dataset <- raw.data
sse <- matrix(nrow=kfold, ncol=7, dimnames=list(1:kfold,c("LogReg", "LR2", "LDA", "QDA", "NNET", "kNN", "LDA1")))

#----------------------------------------------------------------------
#
# Start cross-validation
cvg <- cvidx(dataset,kfold,12345)

for(i in 1:kfold){

# Create training data matrix
dtrain <- dataset[cvg$cv.train[[i]],]
dtest  <- dataset[cvg$cv.test[[i]],]
ntest <- dim(dtest)[1]

#-----------------------------
# logistic regression
#-----------------------------
logreg.lin <- glm(y~., data = dtrain, family = binomial)
pred.logreg <- (predict(logreg.lin,newdata=dtest[,1:3],type="response") > 0.5)
sse[i,1] <- sum((dtest$y-as.numeric(pred.logreg))^2)/ntest

#-----------------------------
# LR2
#-----------------------------
logreg.best <- glm(y~x.1+I(x.2^2), data=dtrain, family=binomial)
pred.steplog <- (predict(logreg.best,newdata=dtest[,1:3],type="response") > 0.5)
sse[i,2] <- sum((dtest$y-as.numeric(pred.steplog))^2)/ntest

#-----------------------------
# linear discriminant analysis
#-----------------------------
train.lda <- lda(y~x.1+I(x.2^2),data=dtrain)
pred.lda <- predict(train.lda,dtest[,1:3])
sse[i,3] <- sum((dtest$y-as.numeric(pred.lda$class)+1)^2)/ntest

#-----------------------------
# quadratic discriminant analysis
#-----------------------------
train.qda <- qda(y~.,data=dtrain)
pred.qda <- predict(train.qda,dtest[,1:3])
sse[i,4] <- sum((dtest$y-as.numeric(pred.qda$class)+1)^2)/ntest

#-----------------------------
# neural network
#-----------------------------
set.seed(123456)
yy <- class.ind(dtrain$y)
train.nnet <- nnet(y~x.1+I(x.2^2), data=dtrain, skip=T, entropy=T, size=4, decay=0.01, maxit=1000, trace=0)
pred.nnet <- predict(train.nnet, dtest[,1:3], type="class")
sse[i,5] <- sum((dtest$y-as.numeric(pred.nnet))^2)/ntest

#-----------------------------
# k-nearest neighbor
#-----------------------------
pred.knn <- knn(dtrain, dtest, dtrain$y, k=10, prob=T)
sse[i,6] <- sum((dtest$y-as.numeric(pred.knn)+1)^2)/ntest

#-----------------------------
# SVM (replaced by LDA1)
#-----------------------------
train.svm <- qda(y~.,data=dtrain)
pred.svm <- (as.numeric(predict(train.svm,dtest[,1:3])$class)-1)
sse[i,7] <- sum((dtest$y-pred.svm)^2)/ntest

}

print(mean(data.frame(sse)))

# Clean up
rm(kfold,dataset,cvg,i,dtrain,dtest,ntest,
  logreg.lin,pred.logreg,
  logreg.best,pred.steplog,
  train.lda,pred.lda,
  train.qda,pred.qda,
  yy,train.nnet,pred.nnet,
  pred.knn,
  train.svm,pred.svm
  )
