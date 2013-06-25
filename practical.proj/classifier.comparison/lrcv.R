#======================================================================
# Perform Logistic Regression with cross validation
#                                              last updated:2006.06.18
#                                                              by TSYo
#======================================================================
#library(MASS)
#attach(raw.data)
# Define parameters
#
kfold <- 2
dataset <- raw.data
sse <- matrix(nrow=kfold, ncol=2, dimnames=list(1:kfold,c("LogReg", "QuaLR")))
bmodel <- NULL

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
logreg.lin <- glm(y~.,data=dtrain,family=binomial)
pred.logreg <- (predict(logreg.lin,newdata=dtest[,1:3],type="response") > 0.5)
sse[i,1] <- sum((dtest$y-as.numeric(pred.logreg))^2)/ntest

#-----------------------------
# Step-wise logistic regression
#-----------------------------
logreg.qua <- glm(y~x.1+I(x.2^2), data=dtrain, family=binomial) 
pred.qualr <- (predict(logreg.qua,newdata=dtest[,1:3],type="response") > 0.5)
sse[i,2] <- sum((dtest$y-as.numeric(pred.qualr))^2)/ntest

#-----------------------------
# Step-wise logistic regression
#-----------------------------
#logreg.best <- stepAIC(logreg.lin,scope=list(lower=~1,upper=~.^2+I(x.1^2)+I(x.2^2)+I(x.3^2)),trace=0)
#pred.steplog <- (predict(logreg.best,newdata=dtest[,1:3],type="response") > 0.5)
#sse[i,2] <- sum((dtest$y-as.numeric(pred.steplog))^2)/ntest
#bmodel <- c(bmodel,list(logreg.best))

}
print(c(mean(sse[,1]),mean(sse[,2])))

# Clean up
rm(kfold,dataset,cvg,i,dtrain,dtest,ntest,
  logreg.lin,pred.logreg,
  logreg.qua,pred.qualr)
