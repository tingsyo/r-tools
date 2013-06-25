# Test the function: optim{stats}
# T.S.Yo 2006.09.29
#
# Test with simple logistic regression, with the log-likelihood function:
#   L(b_0,b_1) = sum{i=1 to n} y_i*(b_0+b_1*x_i) - sum{i=1 to n} ln(1+exp(b_0+b_1*x_i))
# to be maximized
#

logitreg <- function(x ,y, wt = rep(1,length(y)),
                     intercept = T, start = rep(0,p),...)
{
    # Likelihood function
    # plogis : F(x) = 1 / (1 + exp(-(x-m)/s))
    fmin <- function(beta, X, y, w) {
        p <- plogis(X %*% beta)
        -sum(2*w*ifelse(y,log(p),log(1-p)))    
    }
    
    # Fisrt-order derivative of fmin (gradient of likelihood function)
    # dlogis :  f(x) = 1/s exp((x-m)/s) (1 + exp((x-m)/s))^-2
    gmin <- function(beta, X, y, w) {
        eta <- X %*% beta
        p <- plogis(eta)
        #-2*(w*dlogis(eta)*ifelse(y,1/p,-1/(1-p))) %*% X    
        t(-2*(w*dlogis(eta)*ifelse(y,1/p,-1/(1-p)))) %*% X   
    }
     
    #print("Check dimension")
    # Check if dimension exists, if no, make up one
    if(is.null(dim(x))) 
    { dim(x) <- c(length(x),1) }
    dn <- dimnames(x)[[2]]
    
    # Make up dimension names if it's NULL
    if(!length(dn))
    { dn <- paste("Var",1:ncol(x),sep="") }
    p <- ncol(x) + intercept            # p: length of the gradient vector 
    
    if(intercept)
    {
        x <- cbind(1,x)
        dn <- c("(Intercept)",dn)       # dn: dimension names and "Intercept" (if applicable)
    }
    
    if(is.factor(y))
    { y <-(unclass(y) != 1) }           # de-factorise: make 1/0 -> false/true
    
    #print("Optim")
    fit <- optim(start,fmin,gmin, X=x, y=y, w=wt, method="BFGS",...)
    
    #print("Output")
    names(fit$par) <- dn
    cat("\nCoefficients:\n")
    print(fit$par)
    cat("\nResidual Deviance:",format(fit$value),"\n")
    cat("\nConvergence messgae:",format(fit$convergence),"\n")
    invisible(fit)
}

