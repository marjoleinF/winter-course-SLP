rm(list=ls())
cat('\014')

library(ISLR)
library(boot)

###########################################################################################################
###########################################################################################################

# linear regression (on full data set)
plot( Auto$horsepower , Auto$mpg ) # plot suggest a linear effect is not enough
m = lm( mpg ~ horsepower , data=Auto )
summary(m)

# validation set approach (determining test error, but unstable)
 # for model with only linear horsepower
nCases = dim(Auto)[1]
set.seed(11111908)
 # select the training set
train = sample( nCases , nCases/2 )

 # fit model to the training set
m_train = lm( mpg ~ horsepower , data=Auto , subset=train )
summary(m)
summary(m_train)

 # predict the outcome for the validation set based on the coefficients for the training set
mean( ( Auto$mpg - predict( m_train , Auto ) )[-train] ^2 )

 # repeat validation approach for the quadratic model
m2_train = lm( mpg ~ poly(horsepower,2) , data=Auto , subset=train )
mean( ( Auto$mpg - predict( m2_train , Auto ) )[-train] ^2 )

 # repeat validation approach for the cubic model
m3_train = lm( mpg ~ poly(horsepower,3) , data=Auto , subset=train )
mean( ( Auto$mpg - predict( m3_train , Auto ) )[-train] ^2 )

 # let's try (for the fun) polynomials up to order 10
TestError = matrix( -9999 , 1 , 10 ) # pre-allocation
for( tel in 1:10 )
{
  m_train = lm( mpg ~ poly(horsepower,tel) , data=Auto , subset=train )
  TestError[tel] = mean( ( Auto$mpg - predict( m_train , Auto ) )[-train] ^2 )
}

TestError
plot( 1:10 , TestError , type="b" , xlab="order of polynomial" , ylab="estimate of MSE" , main="validation approach" )

###########################################################################################################
###########################################################################################################

# LOOCV for a quadratic model
m2 = glm( mpg ~ poly(horsepower,2) , data=Auto )
m2_loocv = cv.glm( Auto , m2 ) # K is not specified, so K=N (LOOCV)
str(m2_loocv)
m2_loocv$delta # only look at the first element of delta

 # shorter code (for polynomials up to 8)
LOOCV = matrix( -9999 , 1 , 8 )
for( tel in 1:length(LOOCV) )
{
  m = glm( mpg ~ poly(horsepower,tel) , data=Auto )
  m_loocv = cv.glm( Auto , m )
  LOOCV[tel] = m_loocv$delta[1]
  print( tel )
}

LOOCV # view the result
plot( 1:length(LOOCV) , LOOCV , type="b" , xlab="order of polynomial" , ylab="MSE estimate" , main="LOOCV" )

###########################################################################################################
###########################################################################################################

# 10-fold cross-validation (polynomials up to order 8)
CV = matrix( -9999 , 1 , 8 )
for( tel in 1:length(CV) )
{
  m = glm( mpg ~ poly(horsepower,tel) , data=Auto )
  m_cv = cv.glm( Auto , m , K=10 ) # K=10 (10-fold)
  CV[tel] = m_cv$delta[1]
}
CV
plot( 1:length(CV) , CV , type="b" , xlab="order of polynomial" , ylab="estimate of MSE" , main="10-fold Cross-Validation" )

###########################################################################################################
###########################################################################################################

# bootstrap (good for estimation of SE of a parameter, not for determining test error)

 # standard regression analysis (gives an SE to which we can compare the bootstrap SE)
m2 = glm( mpg ~ poly(horsepower,2) , data=Auto )

 # make a function that fits the model on the bootstrap sample, with index a vector of length N with the indices of the selected/sampled cases
regress.fn <- function( dataset , index )
{
  m = glm( mpg ~ poly(horsepower,2) , data=dataset , subset=index )
  return( coef( m ) )
}

 # do bootstrap with 1000 samples
boot( Auto , regress.fn , R=1000 )

# compare bootstrap SE with SE from original analysis
summary(m2)$coef

 # you can also do this for another model
regress.fn2 <- function( dataset , index )
{
  m = glm( mpg ~ horsepower , data=dataset , subset=index )
  return( coef( m ) )
}

boot( Auto , regress.fn2 , R=1000 )

###########################################################################################################
###########################################################################################################

## permutation test: see exercises

###########################################################################################################
###########################################################################################################