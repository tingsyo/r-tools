#======================================================================
# Perform Logistic Regression
#                                              last updated:2006.06.18
#                                                              by TSYo
#======================================================================
#library(MASS)
attach(raw.data)

# Full 1st order model y~ [1+exp(b0+b1*x.1+b2*x.2+b3*x.3)]^-1
logreg.lin <- glm(y~.,data=raw.data,family=binomial)

# Step AIC
logreg.best <- stepAIC(logreg.lin,scope=list(lower=~1,upper=~.^2+I(x.1^2)+I(x.2^2)+I(x.3^2)),trace=0)

# Predict from the best AIC
pred.logreg <- as.numeric(predict(logreg.best,newdata=raw.data[,1:3],type="response") > 0.5)

# Cross tables 
tab.lin <- table(y,as.numeric(predict(logreg.lin,newdata=raw.data[,1:3],type="response") > 0.5))
tab.best <- table(y,as.numeric(predict(logreg.best,newdata=raw.data[,1:3],type="response") > 0.5))
print("Cross table for:")
print("Linear Model");print(tab.lin)
print("Best Model");print(logreg.best);print(tab.best)
print(c("MSPE = ",(tab.best[1,2]+tab.best[2,1])/sum(tab.best)))

# Plot
plot(x.1,x.2,pch=as.character(y),
     xlim=c(-3,3), ylim=c(-2,5), cex=0.7)
x1p <- seq(-3,3,len=100)
x2p <- seq(-2,5,len=100)
conn.points <- expand.grid(x.1=x1p, x.2=x2p)
logreg.z <- predict(logreg.best,conn.points,type="response")*2-1

#points(x.1,x.2,pch=as.character(pred.logreg), col="blue", cex=0.5)
contour(x1p, x2p, matrix(logreg.z,100), add=TRUE, levels=0, labelcex=0)

# Clean up
detach(raw.data)
rm(logreg.lin,logreg.best,pred.logreg,
   tab.lin,tab.best,
   x1p,x2p,conn.points,logreg.z)
