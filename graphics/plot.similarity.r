#=======================================================================
# R functions for plotting similarity matrix
#=======================================================================
# Apache License v.2
# T.S.Yo 2008.08.20
#=======================================================================

#=======================================================================
simplot.shaded <- function(data,ncolors=100,log=F,valuerange=NULL...){
#=======================================================================
# Description: Plot a shaded similarity matrix
#-----------------------------------------------------------------------
# Input:
#   data    : numeric matrix, a similarity matrix
#   ncolors : integer, number of color levels
#   log     : logical, flag for logarithmic scale
#   ...     : other arguments for image.default()
#-----------------------------------------------------------------------
# Output:
#   No return values but a a plot in the graphic device.
#-----------------------------------------------------------------------
# Depend:
#   fields
#=======================================================================
    # Load dependent libraries
    library(fields)
    # Determine scale
    if(log){
        sim <- log10(data)
        sim[which(sim == -Inf)] <- NaN      # Assign log(0) as NaN
    } else {
        sim <- data
    }
    # Get max/min for color range
    if (is.null(valuerange)){
        zmax <- max(sim,na.rm=T)
        zmin <- min(sim,na.rm=T)
    } else {
        zmax <- valuerange[1]
        zmin <- valuerange[2]
    }
    print(c(zmax,zmin))
    #print(seq(zmin,zmax,zinc))
    # Remove the main diagnal
    if(log){
        for (i in 1:dim(data)[1]) {sim[i,i] = zmin}
        colors <- gray(seq(ncolors,1,-1)/ncolors)
        sim[which(sim==NaN)] <- -Inf        # Replace NaN abck to log(0)
    } else {
        for (i in 1:dim(data)[1]) {sim[i,i] = 0}
        colors <- gray(seq(ncolors,1,-1)/ncolors)
    }
    # Basic plot
    image.plot(sim,col=colors, nlevel=ncolors,axes=F, add=F, legend.only=F,zlim=c(zmin,zmax))
    image.default(sim,col=colors,axes=F,add=T,zlim=c(zmin,zmax))
    box(col="black")
    # Axis
    ndim <- dim(sim)[1]
    axis(1,at=(0:(ndim-1)/(ndim-1)),1:ndim,cex.axis=0.6)
    axis(2,at=(0:(ndim-1)/(ndim-1)),1:ndim,cex.axis=0.6)
    #
    #return
}

#=======================================================================
simplot.circle <- function(sim, nlev=5, displev=3,legend=F){
#=======================================================================
# Description: Represent a similarity matrix with a network graph.
#              Variables are placed on a circle as a node, and the arcs
#              indicates the similarity (by color and width)
#-----------------------------------------------------------------------
# Input:
#   sim     : numeric matrix, a similarity matrix
#   nlev    : integer, number of levels to interpolate
#   displev : integer, number of levels to display
#   legend  : logical, a flag for print variable names
#-----------------------------------------------------------------------
# Output:
#   No return values but a a plot in the graphic device.
#-----------------------------------------------------------------------
# Depend:
#   none
#=======================================================================
    # Convert numeric values into levels
    numeric2level <- function(cmx, nlevel=5, threshold=F, thvalue=0.0){
        idx <- NULL
        # Ignore values below threshold and the diagnol
        if(threshold){
            idx <- which(cmx < thvalue)
        }
        diag(cmx) <- min(cmx)   # Assign the diagnol to minimum before looking for
        maxc <- max(cmx)        # Maximum
        cmx[idx] <- maxc        # assign the ignored entries to maxium and
        diag(cmx) <- maxc       # assign diagnol entries to maximum brefore looking for
        minc <- min(cmx)        # Minimum
        cmx[idx] <- minc        # assign ignored entries to the minimum
        clev <- as.integer((cmx-minc)/(maxc-minc)*(nlevel-1))+1     # linear interpolation to [1,nlev]
        clev[idx] <- 0       # assign ignored entries to 0s
        dims <- dim(cmx)
        return(array(clev,dim=dims))
    }
    simlev <- numeric2level(sim,nlevel=nlev,threshold=T,thvalue=-5)
    palette(gray(seq((nlev-1),0,-1)/nlev))
    # Plot nodes as a ciecle on the graph
    n <- dim(sim)[1]
    ang <- (2*pi/n) * (0:(n-1))
    x <- cos(ang)
    y <- sin(ang)
    plot(cbind(x,y),type="p",axes=F,xlab="",ylab="",xlim=c(-1.2,1.8),ylim=c(-1.2,1.2))
    # Mark the vertices
    xt <- x*1.05
    yt <- y*1.05
    text(xt,yt,as.character(1:n))
    # Create in and out point for each vertices
    da <- (2*pi/n)/(20-log(n))
    xin <- cos(ang-da)*0.98
    yin <- sin(ang-da)*0.98
    xout <- cos(ang+da)*0.98
    yout <- sin(ang+da)*0.98
    # Link vertices with arrow
    for(i in 1:n){ for (j in 1:n){
        if ( (i != j) &&  (simlev[i,j] > (nlev-displev)) ){
            arrows(xout[i],yout[i],xin[j],yin[j],length=0.1,angle=15,
                    col=simlev[i,j],lwd=(simlev[i,j]-displev+1))
        }
    }}
    # List ROIs
    if (legend){
        for (i in 1:n){
            text(1.25,y=((8-i)/8),paste(i,dimnames(sim)[[1]][i]),pos=4,cex=0.7)
        }
    }
    palette("default")
    invisible(simlev)
}

#=======================================================================
simplot.graph <- function(sim, nlev=5, displev=3, position=NULL, legend=F){
#=======================================================================
# Description: Represent a similarity matrix with a network graph.
#              Variables are placed on a circle as a node, and the arcs
#              indicates the similarity (by color and width)
#-----------------------------------------------------------------------
# Input:
#   sim     : numeric matrix, a n-by-n similarity matrix
#   nlev    : integer, number of levels to interpolate
#   displev : integer, number of levels to display
#   position: numeric vector, with dimension of n-by-2, specifying
#             the position of each vertices. (-1,-1)~(1,1)
#   legend  : logical, a flag for print variable names
#-----------------------------------------------------------------------
# Output:
#   No return values but a a plot in the graphic device.
#-----------------------------------------------------------------------
# Depend:
#   none
#=======================================================================
# 1. Discretize the similarity values
    # Convert numeric values into levels
    numeric2level <- function(cmx, nlevel=5, threshold=F, thvalue=0.0){
        idx <- NULL
        # Ignore values below threshold and the diagnol
        if(threshold){
            idx <- which(cmx < thvalue)
        }
        diag(cmx) <- min(cmx)   # Assign the diagnol to minimum before looking for
        maxc <- max(cmx)        # Maximum
        cmx[idx] <- maxc        # assign the ignored entries to maxium and
        diag(cmx) <- maxc       # assign diagnol entries to maximum brefore looking for
        minc <- min(cmx)        # Minimum
        cmx[idx] <- minc        # assign ignored entries to the minimum
        clev <- as.integer((cmx-minc)/(maxc-minc)*(nlevel-1))+1     # linear interpolation to [1,nlev]
        clev[idx] <- 0       # assign ignored entries to 0s
        dims <- dim(cmx)
        return(array(clev,dim=dims))
    }
    simlev <- numeric2level(sim,nlevel=nlev,threshold=T,thvalue=-5)
    palette(gray(seq((nlev-1),0,-1)/nlev))
    n <- dim(sim)[1]

# 2. Arrange the variables on the graph
    # If the position of vertices are not specified, put them onto the circle
    if (is.null(position)){
        # Plot nodes as a ciecle on the graph
        ang <- (2*pi/n) * (0:(n-1))
        x <- cos(ang)
        y <- sin(ang)
        # Mark the vertices
        xt <- x*1.05
        yt <- y*1.05
        # Create in and out point for each vertices
        da <- (2*pi/n)/(20-log(n))
        xin <- cos(ang-da)*0.98
        yin <- sin(ang-da)*0.98
        xout <- cos(ang+da)*0.98
        yout <- sin(ang+da)*0.98
    } else {
        x <- position[,1]
        y <- position[,2]
        xin <- x
        yin <- y-0.01
        xout <- x
        yout <- y+0.01
        # Label position depends on the quadrant
        xt <- x+0.05*(sign(x))
        yt <- y+0.05*(sign(y))
    }

# 3. Plotting
    # Plot vertices
    plot(cbind(x,y),type="p",axes=F,xlab="",ylab="",xlim=c(-1.1,1.7),ylim=c(-1.1,1.1))
    # Mark vertices
    text(xt,yt,as.character(1:n))
    # Link the vertices with arrows
    for(i in 1:n){ for (j in 1:n){
        if ( (i != j) &&  (simlev[i,j] > (nlev-displev)) ){
            arrows(xout[i],yout[i],xin[j],yin[j],length=0.1,angle=15,
                    col=simlev[i,j],lwd=(simlev[i,j]-displev+1))
        }
    }}
    # List ROIs
    if (legend){
        for (i in 1:n){
            text(1.25,y=((8-i)/8),paste(i,dimnames(sim)[[1]][i]),pos=4,cex=0.7)
        }
    }
    palette("default")
    invisible(simlev)
}

test.graph <- function(){
    np <- 9
    ppos <- t(array(c(-1,1, 0,1, 1,1,  -1,0, 0,0, 1,0,  -1,-1, 0,-1, 1,-1),dim=c(2,9)))
    sim <- array(runif(81),dim=c(9,9))
    simplot.graph(sim, position=ppos)
}
