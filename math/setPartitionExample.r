#===========================================================================
# A simulation of the set partition problem:
#  Given 8 members to form 4 groups with a size of 2, how many different
#  combination are there?
#===========================================================================
setpart<-function(ntrial=1000, verbose=F){
    col <- NULL
    for (i in 1:ntrial){
        # Randomly sort 1~8
        samp<-sample(1:8)
        # Group every two digits (and sort them in ascending order) -> forming groups
        s1<-sort(samp[1:2])
        s2<-sort(samp[3:4])
        s3<-sort(samp[5:6])
        s4<-sort(samp[7:8])
        # Combine each group and sort by their first element -> forming a unique grouping
        p1<-rbind(s1,s2,s3,s4)
        ord <- order(p1[,1])
        samp <- c(t(p1[ord,]))
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
            isNew = TRUE
            for (j in 1:dim(col)[1]){
                if(sum(col[j,]==samp)==8) {
                    isNew = FALSE
                    break
                }
            }
            if(isNew){
                if (verbose) {cat("new")}
                col <- rbind(col,samp)
            }
        }
        if (verbose) {cat("\n")}
    }
    # Print out summary
    cat("Summary:\n")
    cat(paste("  Total possible ordering of 8 digits:",factorial(8),"\n"))
    cat(paste("  Total trials simulated:",ntrial,"\n"))
    cat(paste("  Number of distinct partitions found:",dim(col)[1],"\n"))
    invisible(col)
}