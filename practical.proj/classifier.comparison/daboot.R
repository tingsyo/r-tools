#======================================================================
# Script: Perform Linear/Quadratic Discriminant Analysis with bootstrap
#                                              last updated:2006.06.25
#                                                              by TSYo
#======================================================================
#library(MASS)
#attach(raw.data)

# Define parameters
#
kfold <- 500
dataset <- raw.data
sse <- matrix(nrow=kfold, ncol=4, dimnames=list(NULL,c("LDA1", "LDA2", "QDA1", "QDA2")))

#----------------------------------------------------------------------
# Create bootstrapping training sample
set.seed(54321)
samp <- NULL
for(i in 1:kfold){
  samp <- c(samp,list(sample(200,200,replace=T)))
}
#----------------------------------------------------------------------
#
# Start cross-validation
for(i in 1:kfold){

# Create training data matrix
dtrain <- dataset[samp[[i]],]
dtest  <- dataset[-samp[[i]],]
ntest <- dim(dtest)[1]

#-----------------------------
# linear discriminant analysis
#-----------------------------
train.lda <- lda(y~.,data=dtrain)
pred.lda <- predict(train.lda,dtest[,1:3])
sse[i,1] <- sum((dtest$y-as.numeric(pred.lda$class)+1)^2)/ntest

train.lda <- lda(y~x.1+I(x.2^2),data=dtrain)
pred.lda <- predict(train.lda,dtest[,1:3])
sse[i,2] <- sum((dtest$y-as.numeric(pred.lda$class)+1)^2)/ntest

#-----------------------------
# quadratic discriminant analysis
#-----------------------------
train.qda <- qda(y~x.1+I(x.2^2),data=dtrain)
pred.qda <- predict(train.qda,dtest[,1:3])
sse[i,3] <- sum((dtest$y-as.numeric(pred.qda$class)+1)^2)/ntest

train.qda <- qda(y~.,data=dtrain)
pred.qda <- predict(train.qda,dtest[,1:3])
sse[i,4] <- sum((dtest$y-as.numeric(pred.qda$class)+1)^2)/ntest

}

print(mean(data.frame(sse)))

# Clean up
rm(kfold,dataset,samp,i,dtrain,dtest,ntest,
  train.lda,pred.lda,
  train.qda,pred.qda
  )
