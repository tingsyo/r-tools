# Test the function: optim{stats}
# T.S.Yo 2006.09.29
#

fdum <- function(x) {   ## Rosenbrock Banana function
    plogis(1+x)
}

set.seed(12345)
dummy.data$x <- runif(101)*10-5
dummy.data$y <- fdum(dummy.data$x)
for(i in 1:101){
  dummy.data[i,3] <- 
  as.factor(ifelse(dummy.data[i,2] > runif(1), 1, 0))
}
plot(dummy.data[,-2])
dummy.data

print("========================================")
print("GLM in R-base")
print("========================================")
fitglm <- glm(class~x,data=dummy.data,family=binomial)
print(fitglm)

print("========================================")
print("Self implemented logistic-regression")
print("========================================")
logitreg(dummy.data$x,dummy.data$class)

rm(fitglm,i)
