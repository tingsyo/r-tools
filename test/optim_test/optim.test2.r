# Test the function: optim{stats}
# T.S.Yo 2006.10.01
#

print("========================================")
print("GLM in R-base")
print("========================================")
fitglm <- glm(low~.,data=bwt,family=binomial)
print(fitglm)

print("========================================")
print("Self implemented logistic-regression")
print("========================================")
options(contrasts=c("contr.treatment","contr.poly"))
X <- model.matrix(low~., data=bwt)[,-1]
logitreg(X, bwt$low)

rm(fitglm)
