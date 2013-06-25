plot((1-uci.out3[3,]),ylim=c(0.4,1),type="b",xaxt="n",xlab="",ylab="Accuracy")
axis(1,1:6,rownames(uci.out))
lines((1-uci.out3[4,]),type="b",lty=2,pch=3)
legend(1.0,0.55,c("PCA+KNN","NCAKNN"),lty=c(1,2),pch=c(1,3),cex=1.0,bty="n")
rect(1,0.47,3.0,0.55)
text(seq(1,6),0.4,labels=as.integer(uci.out3[1,]))
#title("Averaged Accuracy for 10-fold Cross Validation")