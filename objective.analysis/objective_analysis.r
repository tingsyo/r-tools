#=============================================================
# Objective Analysis Functions
#-------------------------------------------------------------
# [Reference]
#   
# [Description]
#   Various functions to plot a vector field
#=============================================================
##-------------------------------
# objana.cressman: Cressman's Objective Analysis
#--------------------------------
# [Reference]
#   http://iridl.ldeo.columbia.edu/dochelp/Documentation/details/index.html?func=%3Acressman
# [Description]
#   Fitting point observations into a grid system using Cressman's objective analysis algorithm
#--------------------------------
objana.cressman <- function(x, y, val, 
    xrange=NULL, yrange=NULL, nx=20, ny=20, minpt=3){
#--------------------------------
# [Parameters]
# - x: the x coordinate of the data point (longitude on map)
# - y: the y coordinate of the data point (latitude on map)
# - val: the value of the data point
# - xrange: (x1,x2), the starting and ending coordinate of x, 
#           by default x1=min(x)-dx, x2=max(x)+dx, dx=(x2-x1)*1.1/nx
# - yrange: (y1,y2), the starting and ending coordinate of y, 
#           by default y1=min(y)-dy, y2=max(y)+dy, dy=(y2-y1)*1.1/ny
# - nx: the number of points on x coordinate (spatial resolution)
# - ny: the number of points on y coordinate (spatial resolution)
# - minpt: the number of data points that must be included within the influence radius of a gridpoint for an analysis value to be calculated for that gridpoint. The default value is 3.
#-------------------------------
  ## Check argument
  npt <- length(x)  # number of data point
  if(npt!=length(y) || npt!=length(val)) 
    stop("The dimensions of input data don't match, abort.")
  ## Analysis of the input data
  # Determine the range of the grid space
  if(is.null(xrange) || length(xrange)!=2){
    dx <- (max(x) - min(x))*1.1/nx
    xmin <- min(x)-2*dx
    xmax <- max(x)+2*dx
  } else {
    xmin <- xrange[1]
    xmax <- xrange[2]
  }
  if(is.null(yrange) || length(yrange)!=2){
    dy <- (max(y) - min(y))*1.1/ny
    ymin <- min(y)-2*dy
    ymax <- max(y)+2*dy
  } else {
    ymin <- yrange[1]
    ymax <- yrange[2]
  }
  # Define distance between grid points
  dx <- (xmax-xmin)/(nx-1)
  dy <- (ymax-ymin)/(ny-1)
  # function for 2d distance
  dist2d <- function(x1,y1,x2,y2){
    return(sqrt
    ((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)))
  }
  # Calculate the average distance between data points
  d.total <- 0
  for(i in 1:(npt-1)){
    for(j in (i+1):npt){
      d.total <- d.total + dist2d(x[i],y[i],x[j],y[j])
    }
  }
  d.mean <- d.total/(npt*(npt-1)/2)
  RR <- 2*d.mean
  ## Create a grid space
  out.val <- matrix(0, ncol=nx, nrow=ny)
  ## 
  for(i in 1:nx){
  for(j in 1:ny){
    # Coordinate of current grid point
    px <- xmin+(i-1)*dx
    py <- ymin+(j-1)*dy
    # Initialization
    ncount <- 0
    c.sum <- 0
    w.sum <- 0
    for(k in 1:npt){  # Loop through data points
      # Calculate distance between grid point and data point
      d <- dist2d(x[k],y[k],px,py)
      # Only consider points within R=2*average-distance-between-actual-data-points
      if(d <= RR){
        ncount <- ncount+1
        w.sum <- w.sum + ((RR*RR-d*d)/(RR*RR+d*d))
        c.sum <- c.sum + ((RR*RR-d*d)/(RR*RR+d*d))*val[k]
      }
    }
    # Set value to 0 if the number of points in range is less than minpt
    if(ncount < minpt){
      out.val[i,j] <- 0
    } else {
      out.val[i,j] <- c.sum/w.sum
    }
  }
  }
  invisible(list(values=out.val, xrange=c(xmin,xmax,dx), yrange=c(ymin,ymax,dy)))
}
#--------------------------------
# objana.cressman: Cressman's Objective Analysis
##-------------------------------

##-------------------------------
# objana.bspline: Objective Analysis using 2D cubic spline
#--------------------------------
# [Reference]
#   
# [Description]
#   Objective Analysis using 2D cubic spline
#--------------------------------
objana.bspline <- function(x, y, val, 
    xrange=NULL, yrange=NULL, nx=20, ny=20, minpt=3){
#--------------------------------
# [Parameters]
# - x: the x coordinate of the data point (longitude on map)
# - y: the y coordinate of the data point (latitude on map)
# - val: the value of the data point
# - xrange: (x1,x2), the starting and ending coordinate of x, 
#           by default x1=min(x)-dx, x2=max(x)+dx, dx=(x2-x1)*1.1/nx
# - yrange: (y1,y2), the starting and ending coordinate of y, 
#           by default y1=min(y)-dy, y2=max(y)+dy, dy=(y2-y1)*1.1/ny
# - nx: the number of points on x coordinate (spatial resolution)
# - ny: the number of points on y coordinate (spatial resolution)
# - minpt: the number of data points that must be included within the influence radius, 3 by default.
# - a1, a2, a3: weights of 3 pass of scanning, by default: 4, 2.5, 1.5
#-------------------------------
  ## Check argument
  npt <- length(x)  # number of data point
  if(npt!=length(y) || npt!=length(val)) 
    stop("The dimensions of input data don't match, abort.")
  ## Analysis of the input data
  # Determine the range of the grid space
  if(is.null(xrange) || length(xrange)!=2){
    dx <- (max(x) - min(x))*1.1/nx
    xmin <- min(x)-2*dx
    xmax <- max(x)+2*dx
  } else {
    xmin <- xrange[1]
    xmax <- xrange[2]
  }
  if(is.null(yrange) || length(yrange)!=2){
    dy <- (max(y) - min(y))*1.1/ny
    ymin <- min(y)-2*dy
    ymax <- max(y)+2*dy
  } else {
    ymin <- yrange[1]
    ymax <- yrange[2]
  }
  # Define distance between grid points
  dx <- (xmax-xmin)/(nx-1)
  dy <- (ymax-ymin)/(ny-1)
  # sub-function for 2d distance
  dist2d <- function(x1,y1,x2,y2){
    return(srqt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)))
  }
  # Calculate the average distance between data points
  d.total <- 0
  for(i in 1:(npt-1)){
    for(j in (i+1):npt){
      d.total <- d.total + dist2d(x[i],y[i],x[j],y[j])
    }
  }
  d.mean <- d.total/(npt*(npt-1)/2)
  RR <- 2*d.mean
  ## Create a grid space
  out.val <- matrix(0, ncol=nx, nrow=ny)
  # sub-function for Cressman interpolation
  cressman.scan <- function(r){
    #
    output <- matrix(0, ncol=nx, nrow=ny)
    #
    for(i in 1:nx){
    for(j in 1:ny){
      # Coordinate of current grid point
      px <- xmin+(i-1)*dx
      py <- ymin+(j-1)*dy
      # Initialization
      ncount <- 0
      c.sum <- 0
      w.sum <- 0
      for(k in 1:npt){  # Loop through data points
        # Calculate distance between grid point and data point
        d <- dist2d(x[k],y[k],px,py)
        # Only consider points within R=2*average-distance-between-actual-data-points
        if(d <= r){
          ncount <- ncount+1
          w.sum <- w.sum + ((r*r-d*d)/(r*r+d*d))
          c.sum <- c.sum + ((r*r-d*d)/(r*r+d*d))*val[k]
        }
      }
      # Set value to 0 if the number of points in range is less than minpt
      if(ncount < minpt){
        output[i,j] <- 0
      } else {
        output[i,j] <- c.sum/w.sum
      }
    }
    }
    return(output)
  }
  ## Run 3 pass
  out.val <- a1*cressman.scan(RR)
  out.val <- out.val + a2*cressman.scan(RR*0.8)
  out.val <- out.val + a3*cressman.scan(RR*0.8*0.8)  
  out.val <- out.val/(a1+a2+a3)
  ## Return results
  invisible(list(values=out.val, xrange=c(xmin,xmax,dx), yrange=c(ymin,ymax,dy)))
}
#--------------------------------
# objana.cressman: Cressman's Objective Analysis
##-------------------------------


