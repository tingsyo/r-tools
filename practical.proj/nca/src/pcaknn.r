########################################################################                    
# Combined PCA-kNN classifier 
# T.S.Yo 2006.11.23
########################################################################                    

pcaknn <- function(train, test, cl, rd=nv,...)
{
    library(class)
    nv <- dim(train)[2]
    pcafit <- prcomp(train)     # PCA function in {stats}
    tm <- pcafit$rotation[,1:rd]                    # Build transformation matrix with first rd principle components
    fl <- sum(pcafit$sdev[1:rd])/sum(pcafit$sdev)   # Proportion of variance explained
    #print(ncafit)
    TrTrain <- train %*% tm     # Transform the training data
    TrTest  <- test  %*% tm     # Transform the testing data
    pred <- knn(TrTrain, TrTest, cl,...)
    return(list(transmatrix=tm, factorloading=fl, predict=pred))
}
