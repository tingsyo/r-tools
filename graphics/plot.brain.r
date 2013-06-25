#=======================================================================
# R functions for plotting the shape of a brain (as a background)
#=======================================================================
# Apache License v.2
# T.S.Yo 2008.11.10
#=======================================================================

#=======================================================================
brainplot.sagittal <- function(xlim=c(-1,1),ylim=c(-1,1), add=FALSE,...){
#=======================================================================
# Description: Plot a shaded similarity matrix
#-----------------------------------------------------------------------
# Input:
#   xlim    : numeric vector, range of x-coord.
#   ylim    : numeric vector, range of y-coord.
#   add     : logical, flag for adding to an existing graph or start a new one.
#   ...     : other arguments for image.default()
#-----------------------------------------------------------------------
# Output:
#   No return values but a a plot in the graphic device.
#-----------------------------------------------------------------------
# Depend:
#=======================================================================
    # Load dependent libraries
    # Define plot device
    if(!add){
        plot(x=NULL,xlim=xlim,ylim=ylim,type="n",axes=F,xlab="",ylab="")
    }
    # Define points
    # ellipse
    t <- seq(-pi,pi,2*pi/500)
    cx <- (xlim[1]+xlim[2])/2
    cy <- (ylim[1]+ylim[2])/2
    a <- abs(xlim[2]-xlim[1])/2
    b <- abs(ylim[2]-ylim[1])/2
    phi <- pi/120
    cat(paste("a:",a,"b:",b,"phi:",phi,"\n"))
    ex <- cx + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
    ey <- cy + b*sin(t)*cos(phi) + a*cos(t)*sin(phi)
    lines(ex,ey)
}

