#======================================================================
# Script: Evalute various models by SSE with crross-validation
#                                              last updated:2006.06.22
#                                                              by TSYo
#======================================================================
# Define parameters
#
kfold <- 10
dataset <- raw.data
#----------------------------------------------------------------------
#
# Start cross-validation
cvg <- cvidx(dataset,kfold,12345)

for(i in 1:kfold){
#----------------------------------------------------------------------
# Create training data matrix
dtrain <- dataset[cvg$cv.train[[i]],]
dtest  <- dataset[cvg$cv.test[[i]],]
#----------------------------------------------------------------------
# logistic regression
logreg.lin <- glm(y~.,data=dtrain,family=binomial)
logreg.best <- stepAIC(logreg.lin,scope=list(lower=~1,upper=~.^2+I(x.1^2)+I(x.2^2)+I(x.3^2)),trace=0)
pred.logreg <- as.numeric(predict(logreg.best,newdata=dtest[,1:3],type="response") > 0.5)
tab.lin <- table(dtest$y,as.numeric(predict(logreg.lin,newdata=dtest[,1:3],type="response") > 0.5))
tab.best <- table(dtest$y,as.numeric(predict(logreg.best,newdata=dtest[,1:3],type="response") > 0.5))
lin.sse <- (tab.lin[1,2]+tab.lin[2,1])/sum(tab.lin)
best.sse <- (tab.best[1,2]+tab.best[2,1])/sum(tab.best)
print("Best Model");print(logreg.best)
print(c("MSPE-lin = ", lin.sse,"   MSPE-best = ", best.sse))




}

# Clean up
rm(kfold,dataset,cvg,i,dtrain,dtest

  )
