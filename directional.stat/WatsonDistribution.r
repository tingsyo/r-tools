#

# Generate a random vector from a Watson distribution of parameter (mu, k)
watsonvector <- function(mu=c(0,0,1), k, rseed=12345){
    # Check unit vector
    if(abs(1-sqrt(mu[1]*mu[1]+mu[2]*mu[2]+mu[3]*mu[3]))>0.000001)
        stop("The mean vector has to be an unit vector!")

    # Functions
    # Convert x-y-z to r-phi-theta
    xyz2unitsphere <- function(x,y,z){
        ifelse(abs(1-sqrt(x*x+y*y+z*z))>0.000001, r<-sqrt(x*x+y*y+z*z), r<-1)
        theta <- acos(z/r)
        if (x==0 && y==0) {
            return(c(theta,0,r))
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
    #
    #
    #
    musph <- xyz2unitsphere(mu[1],mu[2],mu[3])
    mutheta <- musph[1]
    muphi <- musph[2]
    #
    c <- 1/(exp(k)-1)
    while(T){
        # 1
        u <- runif(1)
        v <- runif(1)
        # 2
        s <- 1/k * log(1+u/c)
        # 3
        if(v <= exp(k*s*(s-1))) break
    }
    # Produce sampled direction
    phi <- muphi
    theta <- mutheta + acos(s)
    if(theta>pi){theta <- theta-pi}
    sample <- unitsphere2xyz(theta,phi,1)
    # rotate sample around mu by a random angle, gamma~[0,2pi]
    newv <- function(v,mu){
        gamma <- runif(1)*2*pi
        cg <- cos(gamma)
        sg <- sin(gamma)
        x <- (mu[1]*mu[1]*(1-cg)+cg)*v[1]+
             (mu[1]*mu[2]*(1.0 - cg) - mu[3]*sg) * v[2] +
             (mu[1]*mu[3]*(1.0 - cg) + mu[2]*sg) * v[3]
        y <- (mu[2]*mu[1]*(1.0 - cg) + mu[3]*sg) * v[1] +
             (mu[2]*mu[2]*(1.0 - cg) + cg) * v[2] +
             (mu[2]*mu[3]*(1.0 - cg) - mu[1]*sg) * v[3]
        z <- (mu[3]*mu[1]*(1.0 - cg) - mu[2]*sg) * v[1] +
             (mu[3]*mu[2]*(1.0 - cg) + mu[1]*sg) * v[2] +
             (mu[3]*mu[3]*(1.0 - cg) + cg) * v[3]
        return(c(x,y,z))
    }
    return(newv(sample,mu))
}

# Test the effect of k in Watson's distribution
testwatson <- function(n=1000){
    v0 <- c(0,0,1)
    k1s <- seq(0.0001, 0.01,0.0001)
    k2s <- seq(0.01,1,0.01)
    k3s <- seq(1,100,1)

    dist3d <- function(v1,v2){
        return(sqrt(sum((v1-v2)*(v1-v2))))
    }

    e1 <- array(0,dim=100)
    e2 <- array(0,dim=100)
    e3 <- array(0,dim=100)

    for (i in 1:100){
        for (j in 1:n){
            v1 <- watsonvector(v0,k1s[i])
            v2 <- watsonvector(v0,k2s[i])
            v3 <- watsonvector(v0,k3s[i])
            e1[i] <- e1[i]+dist3d(v0,v1)
            e2[i] <- e2[i]+dist3d(v0,v2)
            e3[i] <- e3[i]+dist3d(v0,v3)
        }
        e1[i] <- e1[i]/n
        e2[i] <- e2[i]/n
        e3[i] <- e3[i]/n
    }

    return(list(en4=e1,en2=e2,e0=e3))
}
