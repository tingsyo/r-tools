#=======================================================================
# R scripts for testing "matrix comparison" functions
#=======================================================================
# Apache License v.2
# T.S.Yo 2008.11.05
#=======================================================================
#=======================================================================
#  Ref:
#   wikipedia: http://en.wikipedia.org/wiki/Mantel_test
#=======================================================================
#-----------------------------------------------
# Load test data
y <- array(c(1,0.25,0.43,0.57,0.25,
             0.25,1,0.25,0.22,0.43,
             0.43,0.25,1,0.57,0.25,
             0.57,0.22,0.57,1,0.57,
             0.25,0.43,0.25,0.57,1),dim=c(5,5))
z <- array(c(1,0.4,0.6,0.73,0.4,
             0.4,1,0.4,0.36,0.60,
             0.6,0.4,1,0.73,0.4,
             0.73,0.36,0.73,1,0.73,
             0.4,0.6,0.4,0.73,1),dim=c(5,5))
#-----------------------------------------------
# Source functions for testing
source("../matrix/mantel.test.r")
source("../matrix/procrustes.analysis.r")

