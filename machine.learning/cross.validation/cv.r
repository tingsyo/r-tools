########################################################################                    
# Create data splits for Cross Validation
# T.S.Yo 2006.11.24
########################################################################                    
cvsp <- function(ndat,kfold=10,rseed=12345,...)
{
#----------------------------------------------------------------------
# Check input
if(missing(ndat))
{
  cat("\nPlease specify the size of the dataset, abort.\n")
  return(NULL)
}
if(missing(kfold))
{  cat("    By default split the data into 10 folds.\n")}
if(missing(rseed))
{  cat("    Default random seed: 12345.\n")}
#----------------------------------------------------------------------
# Create k folds testing sample
  set.seed(rseed)
  ordering <- sample(ndat,ndat)
  samp <- NULL
  npertest <- (ndat%/%kfold)
  nexttest <- (ndat%%kfold)
  iflag <- 1
  # Pick up sample to each fold in turn
  for(i in 1:kfold){
    samp <- c(samp,list(ordering[iflag:(iflag+npertest-1)]))
    iflag <- iflag+npertest
  }
  # Pick up one additional sample for first "nexttest" folds
  if(nexttest!=0){
  for(i in 1:nexttest){
    samp[[i]] <- c(samp[[i]],ordering[iflag])
    iflag <- iflag+1
  }}
#
#----------------------------------------------------------------------
  invisible(samp)
}
