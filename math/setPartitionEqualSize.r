#=======================================================================
# R function for the simulation of the equal-size set partition problem
#  Problem:
#   To partition N objects into M equal-size groups(group size, s=N/M).
#  Functions:
#   - sp.npartition: To compute the number of possible ways of partition.
#       npart = Prod_{i=0}^{M-1}(Combination(N-(i*s),s)) / factorial(M)
#   - sp.list: To list all possible ways of partition.
#   - sp.simulate: To simulate the set-partition prblem of specified N and M
#=======================================================================
# Apache License v.2
# T.S.Yo 2008.11.23
#=======================================================================

#===========================================================================
#   sp.npartition
#===========================================================================
sp.npartition <- function(N,M){
#===========================================================================
# Compute the number of ways to partition N objects into M group of equal size.
#   npart = Prod_{i=0}^{M-1}(Combination(N-(i*s),s)) / factorial(M)
# *Note: M must be dividable by N
#===========================================================================
    #-----------------------------------------
    # Check arguments
    if(missing(N)||missing(M))
        stop("Please specify the number of objects, N, and the number of groups, M.")
    if(N<M)
        stop("N must be greater than M, abort.")
    if(N%%M!=0)
        stop("N objects cannot be divided into M equally-sized groups, abort.")
    #-----------------------------------------
    s <- N/M    # Group size
    #-----------------------------------------
    #  npart = Prod_{i=0}^{M-1}(Combination(N-(i*s),s)) / factorial(M)
    npart <- 1  # Initialize the product
    for (i in 0:(M-1)){
        n_left = N - i*s
        npart = npart * choose(n_left,s)
    }
    npart = npart / factorial(M)
    return(npart)
}

#===========================================================================
#   sp.list
#===========================================================================
sp.list <- function(N,M){
#===========================================================================
# List all possible ways to partition N objects into M group of equal size.
#   npart = Prod_{i=0}^{M-1}(Combination(N-(i*s),s)) / factorial(M)
# *Note: M must be dividable by N
#===========================================================================
    #-----------------------------------------
    # Check arguments
    if(missing(N)||missing(M))
        stop("Please specify the number of objects, N, and the number of groups, M.")
    if(N<M)
        stop("N must be greater than M, abort.")
    if(N%%M!=0)
        stop("N objects cannot be divided into M equally-sized groups, abort.")
    #-----------------------------------------
    s <- N/M    # Group size
    #-----------------------------------------
    #
    npart <- 1  # Initialize the product
    for (i in 0:(M-1)){
        n_left = N - i*s
        npart = npart * choose(n_left,s)
    }
    npart = npart / factorial(M)
    return(npart)
}

#===========================================================================
#   sp.sumulate
#===========================================================================
sp.simulate<-function(N=8,M=4,ntrial=1000, verbose=F){
#===========================================================================
# A simulation of the set partition problem:
#  Given N objects to form M groups each with a size of s, simulate ntrials of random
# permutation 
#===========================================================================
    #-----------------------------------------
    # Check arguments
    if(missing(N)||missing(M))
        stop("Please specify the number of objects, N, and the number of groups, M.")
    if(N%%M!=0)
        stop("M objects cannot be divided into N equally-sized groups, abort.")
    #-----------------------------------------
    s <- N/M    # Group size
    #-----------------------------------------
    col <- NULL
    for (i in 1:ntrial){
        # Randomly sort 1~N
        samp<-sample(1:N)
        # Group every s digits (and sort them in ascending order) -> forming groups
        groups <- NULL
        for (j in 1:M){
            i1 <- (j-1)*s+1
            i2 <- j*s
            groups <- rbind(groups,sort(samp[i1:i2]))
        }
        # Combine each group and sort by their first element -> forming an unique grouping
        ord <- order(groups[,1])
        samp <- c(t(groups[ord,]))
        # Printing out for verbolized illustration
        if (verbose){
            cat(i)
            cat("    ")
            cat(samp)
            cat("  ")
        }
        if (i==1) {
            if (verbose) {cat("new")}
            col <- rbind(col,samp)
        } else {
            # Test if this partition is new
            isNew = TRUE
            for (j in 1:dim(col)[1]){
                if(sum(col[j,]==samp)==N) {
                    isNew = FALSE
                    break
                }
            }
            # Add partition if it is new
            if(isNew){
                if (verbose) {cat("new")}
                col <- rbind(col,samp)
            }
        }
        if (verbose) {cat("\n")}
    }
    rownames(col)<-NULL
    # Print out summary
    cat("Summary:\n")
    cat(paste("  Total possible ordering of 8 digits:",factorial(N),"\n"))
    cat(paste("  Total trials simulated:",ntrial,"\n"))
    cat(paste("  Number of distinct partitions found:",dim(col)[1],"\n"))
    invisible(col)
}

combn <- function (x, m, FUN = NULL, simplify = TRUE, ...) 
{
    stopifnot(length(m) == 1)
    if (m < 0) 
        stop("m < 0")
    if (m == 0) 
        return(if (simplify) vector(mode(x), 0) else list())
    if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == 
        x) 
        x <- seq.int(x)
    n <- length(x)
    if (n < m) 
        stop("n < m")
    m <- as.integer(m)
    e <- 0
    h <- m
    a <- 1:m
    nofun <- is.null(FUN)
    if (!nofun && !is.function(FUN)) 
        stop("'FUN' must be a function or NULL")
    len.r <- length(r <- if (nofun) x[a] else FUN(x[a], ...))
    count <- as.integer(round(choose(n, m)))
    if (simplify) {
        dim.use <- if (nofun) 
            c(m, count)
        else {
            d <- dim(r)
            if (length(d) > 1) 
                c(d, count)
            else if (len.r > 1) 
                c(len.r, count)
            else c(d, count)
        }
    }
    if (simplify) {
        out <- matrix(r, nrow = len.r, ncol = count)
    }
    else {
        out <- vector("list", count)
        out[[1]] <- r
    }
    i <- 2L
    nmmp1 <- n - m + 1L
    while (a[1] != nmmp1) {
        if (e < n - h) {
            h <- 1L
            e <- a[m]
            j <- 1L
        }
        else {
            e <- a[m - h]
            h <- h + 1L
            j <- 1:h
        }
        a[m - h + j] <- e + j
        r <- if (nofun) 
            x[a]
        else FUN(x[a], ...)
        if (simplify) 
            out[, i] <- r
        else out[[i]] <- r
        i <- i + 1L
    }
    if (simplify) 
        array(out, dim.use)
    else out
}