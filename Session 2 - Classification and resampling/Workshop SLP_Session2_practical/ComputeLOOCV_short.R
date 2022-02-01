ComputeLOOCV_short <- function( dataset , models )
{
  LOOCV = matrix( -9999 , 1 , length(models) ) # pre-allocation
  
  for( tel in 1:length(models) )
  {
    m = glm( models[tel] , data=dataset )
    h = lm.influence(m)$h # extract h-values
    resid = residuals(m) # extract residuals
    
    LOOCV[tel] = mean( ( resid / (1-h) ) ^2 )
  }
  
  return( LOOCV ) # return output
}