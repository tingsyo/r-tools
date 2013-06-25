#=======================================================================
# File: apclustering.r
# Author: T.S.Yo 2008.04.29
# Purpose: Perform affinity propagation clustering
#=======================================================================

#=================================================
# Update responsibility.
#   r(i,k) <- s(i,k) - max(a(j,i)+s(i,j)),  where i!=j
#   r(i,i) <- s(i,i) - max(s(i,k)),         where k!=i
#=================================================
updateR <- function(similarity, availability){
    # Dimension
    n <- dim(similarity)[1]
    rNew <- array(dim=c(n,n))
    # Main loop
    for (i in 1:n){
    for (k in 1:n){
        aPs <- array(0,dim=n)   # for a(j,i)+s(i,j) where j!=k
        for (j in 1:n){
            aPs[j] <- availability[j,i] + similarity[i,j]
        }
        aPs[k] <- min(aPs)      # Make sure j!=k
        rNew[i,k] <- similarity[i,k] - max(aPs)
    }
        # Update self-responsibility
        # Retrieve similarity from i to all other points
        scomp <- c(similarity[i,],similarity[,i])
        # Ignore self-similarity
        scomp[c(i,n+i)] <- min(scomp)
        rNew[i,i] <- similarity[i,i] - max(scomp)
    }
    invisible(rNew)
}

#=================================================
# Update availability.
#   a(k,k) <- sum(max{0, r(j,k)}) where j!=k
#   a(k,i) <- min{0, r(k,k)+sum(max{0,r(j,k)})} where j!=i,k
#=================================================
updateA <- function(responsibility){
    # Dimension
    n <- dim(responsibility)[1]
    aNew <- array(dim=c(n,n))
    # Main loop
    for (i in 1:n){
    for (k in 1:n){
        sumRjk <- 0             # for r(i',k) where i'!=k
        # sum(max{0,r(j,k)) where j!=k
        for (j in 1:n){
            sumRjk <- sumRjk + max(0,responsibility[j,k])
        }
        sumRjk <- sumRjk - max(0,responsibility[k,k])
        #
        if (k==i){
            aNew[k,i] <- sumRjk
        } else {
            aNew[k,i] <- min(0, (responsibility[k,k]+sumRjk-max(0,responsibility[i,i])))
        }
    }
    }
    invisible(aNew)
}

#=================================================
# Update exemplars
#    c* <- argmax_k [r(i,k)+a(k,i)]
#=================================================
updateE <- function(responsibility, availability){
    # Dimension
    n <- dim(responsibility)[1]
    eNew <- array(0,dim=n)
    score <- array(0,dim=n)
    # Main loop
    for (i in 1:n){
        for (k in 1:n){
            score[k] <- responsibility[i,k] + availability[k,i]
        }
        eNew[i] <- order(score)[n]
        #print(score)
    }
    invisible(eNew)
}

#=================================================
# Main algorithms for APClustering.
#   * Update responsibility and availability until:
#   * - max-iteration number reached,
#   * - converged (values don't change for conv-iteration).
#=================================================
apclustering <- function(sim, pref=NULL, maxit=500, convit=50, damp=0.5){
    #-----------------------------------
    #

    #-----------------------------------
    # Initialization
    n <- dim(pref)
    itidx <- 0
    #  Initialize responsibility and availability
    r <- array(0,dim=c(n,n))
    a <- array(0,dim=c(n,n))
    #  Check preference and similarity
    if (! is.null(pref)) { for (i in 1:n) { sim[i,i] = pref[i] } }
    #  Initialize exemplars
    exemp <- array(0,dim=n)
    for (i in 1:n) { exemp[i] = i }
    #-----------------------------------
    # Main loop
    convidx <- 0
    while(itidx < maxit){
        # Update R
        newR <- updateR(sim, a)
        for (i in 1:n){for (k in 1:n){
            r[i,k] <- damp*newR[i,k] + (1-damp)*r[i,k]
        }}
        # Update A
        newA <- updateA(r)
        for (i in 1:n){for (k in 1:n){
            a[i,k] <- damp*newA[i,k] + (1-damp)*a[i,k]
        }}
        # Update exemplars
        newE <- updateE(r,a)
        diff <- sum(newE - exemp)
        exemp <- newE
        itidx = itidx + 1
        # Check for convergence
        if (diff == 0) {
            convidx = convidx+1
        } else {
            cat(paste("Iteration:",itidx," -  "))
            cat(exemp)
            cat("\n")
        }
        if (convidx > convit) break
    }
    return(list(itidx,exemp))
}

