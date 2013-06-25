#-------------------------------
# Partial correlation
#  - compute the partial correlation coefficient of rho_xy_z
#-------------------------------
partial.cor <- function(X, ...){
    R <- cor(X)
    RI <- solve(R)
    D <- 1/sqrt(diag(RI))
    R <- -RI * (D %o% D)
    diag(R) <- 0
    rownames(R) <- colnames(R) <- colnames(X)
    return(R)
}
