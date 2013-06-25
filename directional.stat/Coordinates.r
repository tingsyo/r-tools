# Utils
# Convert x-y-z to r-phi-theta
xyz2unitsphere <- function(x,y,z){
    ifelse(abs(1-sqrt(x*x+y*y+z*z))>0.000001, r<-sqrt(x*x+y*y+z*z), r<-1)
    theta <- acos(z/r)
    if (x==0 && y==0) {
        return(c(0,phi,r))
    } else {
        if(y==0){
            ifelse(x > 0, return(c(theta,0,r)), return(c(theta,pi,r)))
        } else { # Avoid divided by zero
            ifelse(y > 0, return(c(theta,0.5*pi,r)), return(c(theta,1.5*pi,r)))
        }
        # For x and y both != 0, check 4 quadrants
        quadrant <- 4-((x/abs(x)+1) + (y/abs(y)+1)/2)
        # (+,+)=1, (+,-)=2, (-,+)=3, (-,-)=4
        switch( quadrant,
                theta <- atan(y/x),
                theta <- atan(y/x) + 2*pi,
                theta <- atan(y/x) + pi,
                theta <- atan(y/x) + pi
        )
    }
    return(c(theta,phi,r))
}
# Convert r-phi-theta to x-y-z
unitsphere2xyz <- function(theta,phi,r=1){
    x <- r*sin(theta)*cos(phi)
    y <- r*sin(theta)*sin(phi)
    z <- r*cos(theta)
    return(c(x,y,z))
}