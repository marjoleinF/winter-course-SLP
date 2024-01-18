knn_CV <- function( Xtrain , Ytrain , MaxK , nFolds )
{
  # Xdata is a data frame with cases in the rows and (dependent) variables as columns
  # Ydata is a vector with the dependent variable
  
  nCases = length( Ytrain )
  CVfolds = sample( rep(1:nFolds,length=nCases) , nCases )
  
  cv_errors = matrix( -9999 , nFolds , MaxK )
  for( fold in 1:nFolds )
  {
    for( Ktel in 1:MaxK )
    {
      knn_pred = knn( Xtrain[CVfolds!=fold,] , Xtrain[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=Ktel )
      cv_errors[ fold , Ktel ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
    }
  }
  
  # compute mean classification rate across 10 folds (and make a plot and determine the optimal K)
  mean_cv_errors = apply( cv_errors , 2 , mean )
  optimalK = which.min( mean_cv_errors )
  
  return( optimalK )
}