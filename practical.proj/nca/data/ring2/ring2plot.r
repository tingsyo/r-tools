postscript("ring2.ps")
# Set graph parameters
par(pin=c(2.5,2.5))
par(mfrow=c(1,2))

# Plot the first 2 attributes: 2 rings
par(cex=0.8)
plot(ring2.x[,1:2],pch=as.character(ring2.y),xlim=c(-2,2),ylim=c(-2,2),xlab="X.1",ylab="X.2")
par(cex=1)
title("First two atributes of dataset ring2")

# Plot the tranformed data
par(cex=0.8)
plot(ring2.x%*%ring2.tm,pch=as.character(ring2.y),xlim=c(-100,100),ylim=c(-100,100),xlab="Transformed Coordinate 1",ylab="Transformed Coordinate 2")
par(cex=1)
title("NCA-Rank2 transformation of dataset ring2")
dev.off()
