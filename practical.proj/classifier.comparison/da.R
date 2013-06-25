#======================================================================
# Script: Perform Linear/Quadratic Discriminant Analysis
#                                              last updated:2006.06.22
#                                                              by TSYo
#======================================================================
#library(MASS)
attach(raw.data)
formula <- y~x.1+I(x.2^2)

# LDA
# Full 1st order model
raw.lda <- lda(formula,data=raw.data)
pred.lda <- predict(raw.lda,raw.data[,1:3])
tab.lda <- table(y,pred.lda$class)
print("LDA")
#print(raw.lda)
print(tab.lda)
print(c("MSPE = ",(tab.lda[1,2]+tab.lda[2,1])/sum(tab.lda)))
print(c("MSPE = ",sum((raw.data$y-as.numeric(pred.lda$class)+1)^2)/200))

# QDA
raw.qda <- qda(formula,data=raw.data)
pred.qda <- predict(raw.qda,raw.data[,1:3])
tab.qda <- table(y,pred.qda$class)
print("QDA")
#print(raw.qda)
print(tab.qda)
print(c("MSPE = ",(tab.qda[1,2]+tab.qda[2,1])/sum(tab.qda)))
print(c("MSPE = ",sum((raw.data$y-as.numeric(pred.qda$class)+1)^2)/200))


# Clean up
detach(raw.data)
rm(formula, raw.lda, tab.lda,raw.qda, tab.qda)
rm(pred.lda, pred.qda)
