postscript("norm.ps")
# Set graph parameters
par(pin=c(2.5,2.5))
par(mfrow=c(1,2))

# Plot the first 2 attributes: 2 rings
par(cex=0.8)
plot(norm2d.x[,1:2],pch=as.character(norm2d.y),xlim=c(-5,5),ylim=c(-5,5),xlab="X.1",ylab="X.2")
par(cex=1)
title("Dataset norm2d")

# Plot the tranformed data
par(cex=0.8)
plot(norm2d.x%*%norm2d.tm,pch=as.character(norm2d.y),xlim=c(-40,40),ylim=c(-40,40),xlab="Transformed Coordinate 1",ylab="Transformed Coordinate 2")
par(cex=1)
title("NCA transformation of dataset norm2d")
dev.off()
