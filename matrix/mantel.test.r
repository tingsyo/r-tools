#=======================================================================
# R functions for Mandel test: correlation between two matrices
#=======================================================================
# Apache License v.2
# T.S.Yo 2008.11.05
#=======================================================================
#=======================================================================
#  Ref:
#   wikipedia: http://en.wikipedia.org/wiki/Mantel_test
#=======================================================================

#=======================================================================
mantel.test <- function(m1, m2, nrep=1000, stat="z", rseed=NULL){
#=======================================================================
# Description:
#  The Mantel test is a statistical test of the correlation between two matrices.
#  The matrices must be of the same rank, and in the usual applications, they
#  are matrices of interrelations between the same vectors of objects.
#
#  The test was first published by Nathan Mantel, a biostatistician at the National
#  Institutes of Health, in 1967. Accounts of it can be found in advanced statistics
#  books, e.g. Sokal & Rohlf (1995), pp. 813-819.
#-----------------------------------------------------------------------
# Input:
#   m1    : matrix 1
#   m2    : matrix 2
#   nrep  : number of repeatition on permutation sampling
#   stat  : Mandel statistics to be used in the test.
#           "z"  : use the row values to compute the sum of the cross product
#           "r"  : normalize the matrices and then compute z
#           "rho": nonparametric Spearman rank correlation
#   rseed : seed for random number generator
#-----------------------------------------------------------------------
# Output:
#   mantel.stat     : String, the type of Mantel statistics used.
#   mantel.obs      : Numeric, value of the Mantel statistics between two given matrices.
#   mantel.sample   : Numeric vector, value of each permutation sample's Mantel statistics.
#   p               : the probability that two given matrices are correlated.
#=======================================================================
  #=======================================================================
  # Checking arguments
  #=======================================================================
    #-----------------------------------
    # Checking for required input
    if(missing(m1) || (missing(m2)))
        stop("Please specify two matrices, abort.")
    #-----------------------------------------------
    # Check ranks of two matrices
    dim1 <- dim(m1)
    dim2 <- dim(m2)
    if(! ((dim1[1]==dim2[1]) && (dim1[2]==dim2[2])))
        stop("Two matrices have different ranks, abort.")
    #-----------------------------------------------
    # Check if matrices are square
    if(dim1[1]!=dim1[2])
        stop("The given matrices are not square, abort.")
    #-----------------------------------------------
    # Set random seed
    if(!is.null(rseed)){
        cat(paste("[Parameter] Set random seed as:",rseed,"\n"))
        set.seed(rseed)
    }
  #=======================================================================
  # Supplementary functions
  #=======================================================================
    #===================================================
    # Mandel statistics z
    #===================================================
    mantel.stat.z <- function(m1, m2) {
        #-----------------------------------
        # Checking for arguments
        if(missing(m1) || (missing(m2)))
            stop("Please specify two matrices!")
        #-----------------------------------------------
        # Check ranks of two matrix
        dim1 <- dim(m1)
        dim2 <- dim(m2)
        if(! ((dim1[1]==dim2[1]) && (dim1[2]==dim2[2])))
            stop("Two matrices have different ranks, abort.")
        #-----------------------------------------------
        # z: sum of cross product
        # z = sum_ij(x_ij * y_ij)
        z <- 0
        for (i in 1:dim1[1]){
        for (j in 1:dim1[2]){
            z = z + (m1[i,j] * m2[i,j])
        }
        }
        #
        return(z)
    }
 
    #===================================================
    # Mandel statistics r
    #===================================================
    mantel.stat.r <- function(m1, m2) {
        #-----------------------------------
        # Checking for arguments
        if(missing(m1) || (missing(m2)))
            stop("Please specify two matrices!")
        #-----------------------------------------------
        # Check ranks of two matrix
        dim1 <- dim(m1)
        dim2 <- dim(m2)
        if(! ((dim1[1]==dim2[1]) && (dim1[2]==dim2[2])))
            stop("Two matrices have different ranks, abort.")
        #-----------------------------------------------
        # r: sum of cross product (normalized)
        # Normalize input matrices
        nm1 <- (m1 - mean(m1))/sd(array(m1))
        nm2 <- (m2 - mean(m2))/sd(array(m2))
        #-----------------------------------------------
        # r = sum_ij(normx_ij * normy_ij) / (N-1)
        r <- 0
        for (i in 1:dim1[1]){
        for (j in 1:dim1[2]){
            r = r + (nm1[i,j] * nm2[i,j])
        }
        }
        r = r/(dim1[1]-1)
        return(r)
    }

    #===================================================
    # Mandel statistics rho
    #===================================================
    mantel.stat.rho <- function(m1, m2) {
        #-----------------------------------
        # Checking for arguments
        if(missing(m1) || (missing(m2)))
            stop("Please specify two matrices!")
        #-----------------------------------------------
        # Check ranks of two matrix
        dim1 <- dim(m1)
        dim2 <- dim(m2)
        if(! ((dim1[1]==dim2[1]) && (dim1[2]==dim2[2])))
            stop("Two matrices have different ranks, abort.")
        #-----------------------------------------------
        # rho: nonparametric Spearman rank correlation
        # Convert values to Fractional ranking
        rm1 <- array(rank(m1, ties.method="average"),dim=dim1)
        rm2 <- array(rank(m2, ties.method="average"),dim=dim2)
        #-----------------------------------------------
        # rho = 1 - ( 6*sum(M1-M2)^2 / N(N-1)^2 )
        rho <- 0
        for (i in 1:dim1[1]){
        for (j in 1:dim1[2]){
            rho = rho + (rm1[i,j] - rm2[i,j])*(rm1[i,j] - rm2[i,j])
        }
        }
        rho = 1 - (6*rho / (dim1[1]*(dim1[1]-1)*(dim1[1]-1)))
        return(rho)
    }

    #===================================================
    # Reordering of a distance/similarity matrix
    #===================================================
    matrix.reorder <- function(m1, ord) {
        #-----------------------------------
        # Checking for arguments
        if(missing(m1))
            stop("Please specify the matrix to be permuted, abort.")
        if(dim(m1)[1] != length(ord))
            stop("The dimension of ordering and matrix do not match, abort.")
        #-----------------------------------------------
        # Re-arrange the matrix
        newm <- m1[ord,ord]
#         for (i in 1:length(ord)){for (j in 1:length(ord)){
#             newm[i,j] = m1[ord[i],ord[j]]
#         }}
        #-----------------------------------------------
        # Reorder the matrix
        return(newm)
    }

    #===================================================
    # Generate exhaustive permutation
    #===================================================
    # Modified from Jim Lemon's work
    permute<-function(elem) {
        # Check argument
        if(!missing(elem)) {
        # Sub-function for inserting an element into a vector
        insert.value<-function(vec,newval,pos) {
            if(pos == 1) return(c(newval,vec))
            lvec<-length(vec)
            if(pos > lvec) return(c(vec,newval))
            return(c(vec[1:pos-1],newval,vec[pos:lvec]))
        }
        # Returns a 2x2 matrix [[A,B],[B,A]] at the bottom of the resursion
        if(length(elem) == 2) return(matrix(c(elem,elem[2],elem[1]),nrow=2))
        # Recursion
        last.matrix<-permute(elem[-1])
        # Combine and extend the resursive results
        dim.last<-dim(last.matrix)
        new.matrix<-matrix(0,nrow=dim.last[1]*(dim.last[2]+1),ncol=dim.last[2]+1)
        for(row in 1:(dim.last[1])) {
            for(col in 1:(dim.last[2]+1))
                new.matrix[row+(col-1)*dim.last[1],]<-insert.value(last.matrix[row,],elem[1],col)
            }
        return(new.matrix)
        }
        else cat("Usage: permute(elem)\n\twhere elem is a vector\n")
    }

  #=======================================================================
  # Main algorithm
  #=======================================================================
    #===================================================
    # Define which Mantel statistics to use
    if(stat=="z") {
        cat("[Parameter] Use Mantel statistics z\n")
        eval <- mantel.stat.z
    }
    if(stat=="r") {
        cat("[Parameter] Use standardized Mantel statistics r\n")
        eval <- mantel.stat.r
    }
    if(stat=="rho") {
        cat("[Parameter] Use ranked Mantel statistics rho\n")
        eval <- mantel.stat.rho
    }
    #===================================================
    # Evaluate Mantel statistics
    mantel.obs <- eval(m1,m2)
    #===================================================
    # Permutation test
    nfact <- factorial(dim1[1])
    #-----------------------------------------------
    # Check if exhaustive permutation is possible
    if( nfact < nrep){
        # Exhaustive permutation test
        nrep <- nfact
        cat(paste("[Parameter] Perform exhaustive permutation test,",nrep,"runs\n"))
        ords <- permute(1:dim1[1])
        mantel.sample <- array(0,dim=nrep)
        for(i in 1:nrep){
            newm <- matrix.reorder(m2,ords[i,])
            mantel.sample[i] <- eval(m1,newm)
        }
    } else {
        # Random permutation test
        cat(paste("[Parameter] Perform",nrep,"permutation tests\n"))
        mantel.sample <- array(0,dim=nrep)
        for(i in 1:nrep){
            ord <- sample(dim1[1])
            newm <- matrix.reorder(m2,ord)
            mantel.sample[i] <- eval(m1,newm)
        }
    }
    #-----------------------------------------------
    p <- length(which(mantel.sample >= mantel.obs))/nrep
    cat(paste("Mantel statistics:",mantel.obs,"\n"))
    cat(paste("P-value:",p,"\n"))
    invisible(list(mantel.stat=stat,mantel.obs=mantel.obs,mantel.sample=mantel.sample,p=p))
}