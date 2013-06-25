#======================================================================
# Show the arcing result
#                                              last updated:2006.06.28
#                                                              by TSYo
#======================================================================
#
# Parameters
#par(fin=c(10,6))
#-----------------------------------------------------------
# (1) Error rate for whole data set
plot(er.arcnnet2[1,],er.arcnnet2[2,], xlab="number of emsemble models", ylab="error rate", xlim=c(0,1000), ylim=c(0,0.1), type="l", lty=3)
#
#-----------------------------------------------------------
# (2) Error rate for 100 resampling
# mean
lines(c(10,50,100,250,500,600),er.arcnnet[,1],lty=1)
# stdev
points(c(10,50,100,250,500,600),er.arcnnet[,1]+er.arcnnet[,2],pch=2,cex=0.8)
points(c(10,50,100,250,500,600),er.arcnnet[,1]-er.arcnnet[,2],pch=6,cex=0.8)
lines(c(10,10),c(er.arcnnet[1,1]+er.arcnnet[1,2],er.arcnnet[1,1]-er.arcnnet[1,2]))
lines(c(50,50),c(er.arcnnet[2,1]+er.arcnnet[2,2],er.arcnnet[2,1]-er.arcnnet[2,2]))
lines(c(100,100),c(er.arcnnet[3,1]+er.arcnnet[3,2],er.arcnnet[3,1]-er.arcnnet[3,2]))
lines(c(250,250),c(er.arcnnet[4,1]+er.arcnnet[4,2],er.arcnnet[4,1]-er.arcnnet[4,2]))
lines(c(500,500),c(er.arcnnet[5,1]+er.arcnnet[5,2],er.arcnnet[5,1]-er.arcnnet[5,2]))
lines(c(600,600),c(er.arcnnet[6,1]+er.arcnnet[6,2],er.arcnnet[6,1]-er.arcnnet[6,2]))
# legend
legend(400,0.095,c("error rate for whole data set","error rate for 100 resampling"),lty=c(3,1),cex=0.8,bty="n")
rect(390,0.085,950,0.097)
title("Performance of ARC-X4 Neural Network Classifier")

