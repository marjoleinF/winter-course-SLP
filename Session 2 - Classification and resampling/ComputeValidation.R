ComputeValidation <- function( seed , dataset , outcome , models )
{
  set.seed( seed ) # set seed
  nCases = dim( dataset )[1] # compute number of cases
  train = sample( nCases , nCases/2 ) # split data in training and validation set
  TestError = matrix( -9999 , 1 , length(models) ) # pre-allocation
  for( tel in 1:length(models) )
  {
    ResModel_train = glm( models[tel] , data=dataset , subset=train ) # fit on training set
    TestError[tel] = mean( ( outcome - predict( ResModel_train , dataset ) )[-train] ^2 ) # compute prediction error on validation set
    rm( ResModel_train )
  }
  
  return(TestError) # return output
}