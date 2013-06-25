#======================================================================
# Script: Perform classification via Neural Network 
#                                              last updated:2006.06.22
#                                                              by TSYo
#======================================================================
#library(nnet)
#attach(raw.data)
set.seed(12345)
dataset <- data.frame(raw.data[,1:3], y = c(rep("0",100), rep("1", 100)))

# Generates a class indicator function
yy <- class.ind(raw.data$y)

# Train the n-network
raw.nnet <- nnet(y~., data=raw.data, skip=T, entropy=T,size=4, decay=0.01, maxit=1000)
raw.nnet2 <- nnet(y~x.1+I(x.2^2), data=raw.data, skip=T, entropy=T,size=4, decay=0.01, maxit=1000)

# Predict with trained n-network
# Output
print("Neural-Network")
pred.nnet <- predict(raw.nnet, raw.data[,1:3], type="class")
print(c("MSPE1 = ",sum((raw.data$y-as.numeric(pred.nnet))^2)/(dim(raw.data)[1])))
pred.nnet2 <- predict(raw.nnet2, raw.data, type="raw")
print(c("MSPE2 = ",sum((raw.data$y-as.numeric(pred.nnet2))^2)/(dim(raw.data)[1])))

# Confusion table
#tab.nnet <- table(y,pred.nnet)

#print(summary(raw.nnet))
#print(tab.nnet)

# Clean up
#detach(raw.data)
rm(yy,dataset,raw.nnet,raw.nnet2,pred.nnet,pred.nnet2)
