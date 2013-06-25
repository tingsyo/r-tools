########################################################################                    
# Function for Neighbourhood Components Analysis classifier v.4
# T.S.Yo 2006.11.23
#  - change initial guess options: both eigen decomposition and Cholesky decomposition
########################################################################                    
nca <- function(x ,y, rd = nv, wt = rep(1,length(y)), start = "RAND", rseed=12346,...)
{
########################################################################                   
# Description:
#-----------------------------------------------------------------------
# Input:
#   x     : a matrix (array) that each row represent a data point
#   y     : a factor contains class labels for each row in x
#   rd    : reduced dimension, set to 2 by default
#   wt    : weightings for each row in x
#   start : initial guess of transformation matrix
#           "COVM": square root of inverse covariance matrix
#           "PCOVM": pooled COVM by class labels
#           "RAND": random matrix
#-----------------------------------------------------------------------
# Output:
#   par   : the transformation matrix
#   value : the last value of function to be optimized (minimized)
#   convergence: (copy from return of function 'optim')
#           0 indicates successful convergence. Error codes are
#           1 indicates that the iteration limit maxit had been reached.
########################################################################                    

########################################################################                    
# Supplementary functions
########################################################################                    
    # Compute the Eclidean distance^2 matrix for two set of vectors
    # DistMtx(i,j) = Distance[(ith-row in mtx1),(jth-row in mtx2)]
    #
    DistMtx <- function(mtx1,mtx2)
    {
        # Check dimensions before computation
        if(dim(mtx1)[2]!=dim(mtx2)[2])
        {
            print("2 sets of vectors are not in the same dimension, abort.")
            return(NULL)
        }
        
        # Create a matrix contains the Euclidean distance^2 between
        #  each pair {(i,j)|i in mtx1, j in mtx2}.
        n1 <- dim(mtx1)[1]      # number of data in mtx1
        n2 <- dim(mtx2)[1]      # number of data in mtx2
        DistM <- matrix(nrow=n1,ncol=n2)    # For output
        
        for(i in 1:n1){         # dist = sum((x1i-x2i)^2)
        for(j in 1:n2){
            DistM[i,j] <- sum((mtx1[i,]-mtx2[j,])^2)
        }
        }
        
        return(DistM)
    }

    # Objective function
    fmin <- function(A,X,y,w=rep(1,nd))
    {
        nd <- dim(X)[1]                 # dimension: number of data points
        nv <- length(A)/dim(X)[2]       # dimension: number of variables
        AX <- X %*% matrix(A,ncol=nv)   # linear-transform
        expdm <- exp(-DistMtx(AX,AX))   # exp(-d_ij)
        p_ij <- matrix(ncol=nd,nrow=nd) # p_ij = expdm_ij/sum_k(expdm_ik)
        p_i  <- array(dim = nd)         # p_i = sum(j) p_ij

        sumd <- 0                       # sum_k(expdm_ik)
        for(i in 1:nd){
            sumd <- sum(expdm[i,-i])
        for(j in 1:nd){
            p_ij[i,j] <- expdm[i,j]/sumd
        }
            p_ij[i,i] <- 0
            p_i[i] <- sum(p_ij[i,which(y==y[i])])
        }
        
        # phi = sum_i(p_i)
        phi <- sum(p_i)
        
        return(-phi)
    }
    
    # Gradient of Obj function
    gmin <- function(A,X,y,w=rep(1,nd))
    {
        nd <- dim(X)[1]                 # dimension: number of data points
        nv <- length(A)/dim(X)[2]       # dimension: number of variables
        AX <- X %*% matrix(A,ncol=nv)   # linear-transform
        expdm <- exp(-DistMtx(AX,AX))   # exp(-d_ij)
        p_ij <- matrix(ncol=nd,nrow=nd) # p_ij = expdm_ij/sum_k(expdm_ik)
        p_i  <- array(dim = nd)         # p_i = sum(j) p_ij

        sumd <- 0                       # sum_k(expdm_ik)
        for(i in 1:nd){
            sumd <- sum(expdm[i,-i])
        for(j in 1:nd){
            p_ij[i,j] <- expdm[i,j]/sumd
        }
            p_ij[i,i] <- 0
            p_i[i] <- sum(p_ij[i,which(y==y[i])])
        }

        # phi' = 2A*sum_i{p_i*sum_k(p_ik*x_ik^2) - sum_j(p_ij*x_ij^2)}
        tmpm <- p_ij*DistMtx(X,X)
        tmp1 <- 0
        tmp2 <- 0
        tmp3 <- 0
                
        for(i in 1:nd){
            tmp1 <- p_i[i]*sum(tmpm[i,])
            tmp2 <- sum(tmpm[i,which(y==y[i])])
            tmp3 <- tmp3 + tmp1-tmp2
        }
        
        # Return value
        return(-tmp3*2*A)            
    }

    # Perplexity as the estimated K for kNN
    perplexity <- function(A,X)
    {
        nd <- dim(X)[1]                 # dimension: number of data points
        nv <- length(A)/dim(X)[2]       # dimension: number of variables
        AX <- X %*% A                   # linear-transform
        expdm <- exp(-DistMtx(AX,AX))   # exp(-d_ij)
        p_ij <- matrix(ncol=nd,nrow=nd) # p_ij = expdm_ij/sum_k(expdm_ik)

        sumd <- 0                       # sum_k(expdm_ik)
        for(i in 1:nd){
            sumd <- sum(expdm[i,-i])
        for(j in 1:nd){
            p_ij[i,j] <- expdm[i,j]/sumd
        }
            p_ij[i,i] <- 0
        }
        perplexity <- exp(-sum(p_ij*log(p_ij),na.rm=T)/nd)
        return(perplexity)
    }
    
    
    
    # Square root of inverse covariance matrix
    csricov <- function(x,rd)
    {
        inig <- t(chol(solve(cov(x))))  # Cholesky square root
        return(inig[,1:rd])
    }

    # Square root of inverse pooled covariance matrix
    csripcov <- function(x,y,rd)
    {
        nv <- dim(x)[2]                 # dimension of covariance matrix
        cls <- unique(y)                # class labels
        nclass <- length(cls)           # number of classes
        pcov <- diag(nv)-diag(nv)
        for(i in 1:nclass)
        {
            pcov <- pcov + cov(x[which(y==cls[i]),])
        }
        pcov <- pcov/nclass
        inig <- t(chol(solve(pcov)))    # Cholesky square root
        return(inig[,1:rd])
    }
        
    # Square root of inverse covariance matrix
    esricov <- function(x,rd)
    {
        tmp <- eigen(solve(cov(x)))     # eigen decomposition
        inig <- (tmp$vectors %*% diag(sqrt(tmp$values))) %*% solve(tmp$vectors)
        return(inig[,1:rd])
    }

    # Square root of inverse pooled covariance matrix
    esripcov <- function(x,y,rd)
    {
        nv <- dim(x)[2]                 # dimension of covariance matrix
        cls <- unique(y)                # class labels
        nclass <- length(cls)           # number of classes
        pcov <- diag(nv)-diag(nv)
        for(i in 1:nclass)
        {
            pcov <- pcov + cov(x[which(y==cls[i]),])
        }
        pcov <- pcov/nclass
        tmp <- eigen(solve(pcov))
        inig <- (tmp$vectors %*% diag(sqrt(tmp$values))) %*% solve(tmp$vectors)
        return(inig[,1:rd])
    }

########################################################################                    
# End of suppliment functions
########################################################################                    
    
########################################################################                    
# Main function
########################################################################                    
    nv <- ncol(x)            # nv: number of variables, also the rank of transform matrix A

    # Check if reduced dimension less than original
    if(rd > nv) 
    {
        cat("\nSpecified dimension higher than the original, abort.\n")
        return(NULL)
    }

    # Check if dimension exists, if no, make up one
    if(is.null(dim(x))) 
    { dim(x) <- c(length(x),1) }
            
    if(is.factor(y))
    { y <-(unclass(y)) }     # de-factorise

    # Build initial guess
    set.seed(rseed)
    inig <- matrix(runif(nv*rd),ncol=rd)        # RAND by default    
    if(start=="COVMC"){inig <- csricov(x,rd)}     # if "COVMC" 
    if(start=="PCOVMC"){inig <- csripcov(x,y,rd)} # if "PCOVMC"
    if(start=="COVME"){inig <- esricov(x,rd)}     # if "COVME" 
    if(start=="PCOVME"){inig <- esripcov(x,y,rd)} # if "PCOVME"
    
    # Use optim to find parameters that minimize fmin with gmin 
    fit <- optim(array(inig),fmin,gmin, X=x, y=y, w=wt, method="BFGS",...)
    
    # Output
    tm <- matrix(fit$par,ncol=rd)
    perp <- perplexity(tm,x)
    #cat("\nTransformation Matrix:\n")
    #print(tm)
    #cat("\nFinal function value:",format(fit$value),"\n")
    #cat("\nConvergence messgae:",format(fit$convergence),"\n\n")
    return(list(par=tm, perplexity=perp, value=format(fit$value), convergence=format(fit$convergence)))
########################################################################                    
# End of main function
########################################################################                    
}
