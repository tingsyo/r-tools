# std.data.R
# - T.S.Yo 2006.09.26
# - Source a function, std.data(x), which returns a standardized
#   data of given array x.
#
std.data <- function(x)
{
########################################################################                    
# Supplementary functions
########################################################################                    
  # Standarize 1-d array
  std1d <- function (y)
  {
    # Get dimensions of given data
    lx <- length(y)
    mx <- mean(y)
    sdx <- sd(y)

    # Standardized_value = (original_value - sample_mean) / sample_standard_deviation
    xx <- (y - mx)/sdx

    # Return
    return(xx)
  }  
########################################################################                    
# Main function
########################################################################                    
  # Check arguments
  if (!missing(x)) {
    # Check if dimension exists, if no, make up one
    if(is.null(dim(x))) 
    { dim(x) <- c(length(x),1) }
  }
  
  # Get dimension and loop
  stdx <- NULL
  nx <- dim(x)[2]
  for(j in 1:nx){
    stdx <- cbind(stdx,(std1d(x[,j])))
  }
  
  return(stdx)
}
