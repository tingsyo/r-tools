#=======================================================================
# R function to return a set of points  (in Cartician coordinate) 
#  within and on the circle which fills up the n*n square
#=======================================================================
# Apache License v.2
# T.S.Yo 2008.08.26
#=======================================================================

#=======================================
# Read in the Analyze 7,5 hdr/img
circleInSquare <- function(n){
    # Get center and radius
    radius <- (n-1)/2
    center <- c((n+1)/2,(n+1)/2)
    # function to compute Euclidean distance
    ed <- function(p1, p2){
        return(sqrt((p1[1]-p2[1])*(p1[1]-p2[1])+(p1[2]-p2[2])*(p1[2]-p2[2])))
    }
    # Scan through the square
    #  For each of the 4 quadrants (to the center), check the point:
    #   - if it is within the circle, keep it
    #   - if it is outside of the circle:
    #     - if its upper/lower point is inside, interpolate the point on the circle
    #     - if its right/left point is inside, interpolate the point on the circle
    #  The total number of points should be ~ N^2 - (4r^2 - PI*r^2) (= n^2 - (4-PI)r^2)
    points <- NULL
    for(j in 1:n){ for(i in 1:n){
        # If outside of the circle
        if(ed(c(i,j),center)>radius) {
            # Lower/left of the center, check the right/upper grid point
            if((j < center[2])&&(i < center[1]) ){
                if(ed(c(i+1,j),center)<radius){  # if the right grid point is inside
                    px <- -sqrt(radius*radius-(j-center[2])*(j-center[2])) + center[1]
                    points <- rbind(points,c(px,j))
                }
                if(ed(c(i,j+1),center)<radius){  # if the upper grid point is inside
                    py <- -sqrt(radius*radius-(i-center[1])*(i-center[1])) + center[2]
                    points <- rbind(points,c(i,py))
                }
            }
            # Lower/right of the center, check the left/upper grid point
            if((j < center[2])&&(i >= center[1]) ){
                if(ed(c(i-1,j),center)<radius){  # if the left grid point is inside
                    px <- sqrt(radius*radius-(j-center[2])*(j-center[2])) + center[1]
                    points <- rbind(points,c(px,j))
                }
                if(ed(c(i,j+1),center)<radius){  # if the upper grid point is inside
                    py <- -sqrt(radius*radius-(i-center[1])*(i-center[1])) + center[2]
                    points <- rbind(points,c(i,py))
                }
            }
            # Upper/left of the center, check the right/lower grid point
            if((j >= center[2])&&(i < center[1]) ){
                if(ed(c(i+1,j),center)<radius){  # if the right grid point is inside
                    px <- -sqrt(radius*radius-(j-center[2])*(j-center[2])) + center[1]
                    points <- rbind(points,c(px,j))
                }
                if(ed(c(i,j-1),center)<radius){  # if the lower grid point is inside
                    py <- sqrt(radius*radius-(i-center[1])*(i-center[1])) + center[2]
                    points <- rbind(points,c(i,py))
                }
            }
            # Upper/right of the center, check the left/lower grid point
            if((j >= center[2])&&(i >= center[1]) ){
                if(ed(c(i-1,j),center)<radius){  # if the right grid point is inside
                    px <- sqrt(radius*radius-(j-center[2])*(j-center[2])) + center[1]
                    points <- rbind(points,c(px,j))
                }
                if(ed(c(i,j-1),center)<radius){  # if the upper grid point is inside
                    py <- sqrt(radius*radius-(i-center[1])*(i-center[1])) + center[2]
                    points <- rbind(points,c(i,py))
                }
            }
        } else {
             points<-rbind(points,c(i,j))
        }
    }}
    return(points)
}