########################################################################                    
# Combined NCA-kNN classifier 
# T.S.Yo 2006.11.08
########################################################################                    

ncaknn <- function(train, test, cl, k = 1,...)
{
    library(class)
    ncafit <- nca(train,cl,...)
    tm <- ncafit$par            # Transformation matrix trained through NCA
    estk <- as.integer(ncafit$perplexity)
    #print(ncafit)
    TrTrain <- train %*% tm     # Transform the training data
    TrTest  <- test  %*% tm     # Transform the testing data
    pred <- knn(TrTrain, TrTest, cl, k=estk)
    return(list(k=estk, predict=pred))
}
