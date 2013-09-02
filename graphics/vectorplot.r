#=============================================================
# Vector plot
#-------------------------------------------------------------
# [Reference]
#    
# [Description]
#   Various functions to plot a vector field
#=============================================================
##-------------------------------
# Draw arrows to show vectors
##-------------------------------
vecplot.arrow <- function(x1, y1, x2, y2){
  ## Load required library
  require(grid)
  require(ggplot2)
  ## Checking data pairs
  n <- length(x1)
  array.moving <- NULL
  array.static <- NULL
  for(i in 1:n){
    if((x2[i]==x1[i]) && (y2[i]==y1[i])){
      array.static <- c(array.static,i)
    } else {
      array.moving <- c(array.moving,i)
    }
  }
  ## Prepare dataset
  df.all <- cbind(x1,y1,x2,y2)
  df.moving <- data.frame(df.all[as.numeric(array.moving),])
  df.static <- data.frame(df.all[as.numeric(array.static),])
  # Draw arrow on moving points
  vplot <- ggplot(data=df.moving) + 
    xlim(0,1) + ylim(0,1) + # Scale to (0,1)
    theme(axis.title = element_blank(), axis.text = element_blank()) + # Remove labels and text for axis
    aes(x=x1, y=y1) + 
    geom_segment(aes(xend=x2, yend=y2), arrow = arrow(length = unit(0.3,"cm"))) +
    geom_point(data=df.static,aes(x1,y1))
  
  vplot
  #return(df.moving)
}

#
##-------------------------------
# Make animations
##-------------------------------
vecplot.animation <- function(x1, y1, x2, y2, nframe=5, output=F){
  ## check parameters
  npoint <- length(x1)
  if(npoint!=length(y1)||npoint!=length(x2)||npoint!=length(y2)) 
    stop("Vector length doesn't match, abort.")
  ## Calculate frame
  vecs <- array(0,dim=c(npoint, 2, nframe))
  ids <- 1:npoint
  # 
  for(i in 1: nframe){
    vecs[,1,i] <- x1 + (i-1)*(x2-x1)/(nframe-1)
    vecs[,2,i] <- y1 + (i-1)*(y2-y1)/(nframe-1)
    # Create data frame
    dftmp <- data.frame(vecs[,,i])
    dftmp <- cbind(dftmp,1:dim(dftmp)[1])
    names(dftmp) <- c("x","y","id")
    # Output files
    fn <- paste("vplot",i,"png",sep=".")
    if(output) {png(filename=fn)}
    vplot <- ggplot(data=dftmp[,,1]) + 
      xlim(0,1) + ylim(0,1) + # Scale to (0,1)
      theme(axis.title = element_blank(), axis.text = element_blank()) + # Remove labels and text for axis
      geom_point(data=dftmp, aes(x, y, colour=factor(id), size=5, alpha=0.2), position="jitter") +
      theme(legend.position="none")
    print(vplot)
    if(output) {dev.off()}
  }
  invisible(vecs)
}


