ComputeCV_repeated <- function( dataset , models , Kfold , nTimes , distribfamily=gaussian )
{
  source( "ComputeCV.R" )
  
  if( Kfold == 1 ) # stop when K=1 (is useless)
  {
    error( "Kfold should be an integer larger than 1" ) #gives an error message
    stopifnot( Kfold != 1 ) #terminates the function
  }
  
  if( Kfold == dim(dataset)[1] ) # check whether the user wants LOOCV
  {
    nTimes = 1 # in that case, nTimes should be 1
    warning( "you are performing Leave-One-Out Cross-Validation") # give a warning message
  }
  
  CV_all = matrix( -9999 , nTimes , length(models) ) # pre-allocation
  
  for( tel in 1:nTimes )
  {
    print( paste0( "TimeNr = " , tel  ) )
    CV_all[ tel , ] = ComputeCV( dataset , models , Kfold , distribfamily )
  }
  
  return( CV_all ) # return output
}