#-------------------------------
# Running correlation
# http://quantpalaeo.wordpress.com/2013/01/04/running-correlations-running-into-problems/
#  - compute the running correlation between x and y for every window of n observations
#  - assume: each column represent a variable and each row a record
#-------------------------------
running.cor <- function(X, n, ...){
    nrec <- dim(X)[1]
    nvar <- dim(X)[2]
    nrun <- nrec - n + 1
    rcor <- array(0,c(nvar, nvar, nrun))
    dimnames(rcor)[[1]] <- dimnames(rcor)[[2]] <- colnames(X)
    runnames <- NULL
    for(i in 1: nrun){
        tmp <- X[i:(i+n-1),]
        rcor[,,i] <- cor(tmp)
        runnames <- c(runnames,paste(rownames(X)[i],rownames(X)[(i+n-1)],sep="-"))
    }
    dimnames(rcor)[[3]] <- runnames
    return(rcor)
}
