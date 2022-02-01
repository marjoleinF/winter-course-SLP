ComputeCV <- function( dataset , models , Kfold , distribfamily=gaussian )
{
  CV = matrix( -9999 , 1 , length(models) ) # pre-allocation
  for( tel in 1:length(models) )
  {
    print( tel )
    m = glm( models[tel] , data=dataset , family=distribfamily )
    m_cv = cv.glm( dataset , m , K=Kfold )
    CV[tel] = m_cv$delta[1]
    rm( m , m_cv )
  }
  
  return( CV ) # return output
}