plot((1-uci.out2[1,]),ylim=c(0.4,1),type="b",xaxt="n",xlab="",ylab="Accuracy")
axis(1,1:6,rownames(uci.out))
lines((1-uci.out2[2,]),type="b",lty=2,pch=17)
lines((1-uci.out2[3,]),type="b",lty=3,pch=15)
lines((1-uci.out2[4,]),type="b",lty=4,pch=3)

legend(1.0,0.55,
       c("LDA","QDA","1NN","NCA"),
       lty=c(1,2,3,4),
       pch=c(1,17,15,3),
       cex=1.0)

#title("Averaged Accuracy for 10-fold Cross Validation")

