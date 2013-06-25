#======================================================================
# Show the raw data
#                                              last updated:2006.06.18
#                                                              by TSYo
#======================================================================
# Show the raw data
# A 2X2 plot
par(mfrow=c(2,2),mar=c(4,4,1,1)+0.5)

#-----------------------------------------------------------
# (1) x.1-x.2
plot(raw.data$x.1, raw.data$x.2, pch=as.character(raw.data$y),cex=0.5, xlab="x.1", ylab="x.2", xlim=c(-3,3), ylim=c(-2,5))

#-----------------------------------------------------------
# (2) x.1, x.2, x.3 vs y
plot(raw.data$x.1, raw.data$x.2, xlab="Value of X", ylab="X", xlim=c(-3,5), ylim=c(0,1), type="n", axes=FALSE)
axis(1, seq(-3,5,2), seq(-3,5,2))
axis(2, c(0.2,0.4,0.6),c("x.3","x.2","x.1"),lty=0,padj=0)
box()
# legend
rect(-2.5,0.8,3.5,1.0)
text(-1,0.9,"+ : y = 1")
text(2,0.9,"o : y = 0")
# x.1
points(raw.data$x.1, rep(0.6,200),pch=as.character(raw.data$y),cex=0.5)
#x.2
points(raw.data$x.2, rep(0.4,200),pch=as.character(raw.data$y),cex=0.5)
#x.3
points(raw.data$x.3, rep(0.2,200),pch=as.character(raw.data$y),cex=0.5)

#-----------------------------------------------------------
#(3) x.1-x.3
plot(raw.data$x.1, raw.data$x.3, pch=as.character(raw.data$y),cex=0.5, xlab="x.1", ylab="x.3", xlim=c(-3,3), ylim=c(-2,3))


#-----------------------------------------------------------
#(4) x.2-x.3
plot(raw.data$x.2, raw.data$x.3, pch=as.character(raw.data$y),cex=0.5, xlab="x.2", ylab="x.3", xlim=c(-2,5), ylim=c(-3,3))

