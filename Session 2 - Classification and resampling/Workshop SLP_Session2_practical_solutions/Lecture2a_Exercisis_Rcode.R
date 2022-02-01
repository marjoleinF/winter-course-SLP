rm(list=ls())
cat('\014')

library(boot)
library(ISLR)

student = read.csv2( "student-mat.csv" )
student$G1bin = student$G1>8 # to make a dichotomous outcome
table(student$G1bin) # check the number of cases in each class of the dichotomous outcome

  # select the relevant variables from the data set
student = student[ c("G1","G1bin","studytime","schoolsup","romantic") ]
View(student)

###########################################################################################################
###########################################################################################################

##############
# test error #
##############

# validation set approach (determining test error, but unstable)
nStudents = dim(student)[1]
set.seed( 1234 )
train = sample( nStudents , nStudents/2 )

# fit model only on training data
m4_train = glm( G1 ~ studytime * schoolsup , data=student , subset=train )

# check that different estimates for the coefficients are obtained
summary(m4_train)

# predict the outcome for the cases in the validation set, using the coefficient from the training set
# compute mean squared error
mean( ( student$G1 - predict( m4_train , student ) )[-train] ^2 )


####################
# model complexity #
####################

# we will use the same split in test and validation set

TestError = vector( length = 6 , mode = "list" ) # pre-allocation

m1_train = glm( G1 ~ 1 , data=student , subset=train )
TestError[1] = mean( ( student$G1 - predict( m1_train , student ) )[-train] ^2 )

m2_train = glm( G1 ~ studytime , data=student , subset=train )
TestError[2] = mean( ( student$G1 - predict( m2_train , student ) )[-train] ^2 )

m3_train = glm( G1 ~ studytime + schoolsup , data=student , subset=train )
TestError[3] = mean( ( student$G1 - predict( m3_train , student ) )[-train] ^2 )

TestError[4] = mean( ( student$G1 - predict( m4_train , student ) )[-train] ^2 )

m5_train = glm( G1 ~ studytime * schoolsup + romantic , data=student , subset=train )
TestError[5] = mean( ( student$G1 - predict( m5_train , student ) )[-train] ^2 )

m6_train = glm( G1 ~ studytime * schoolsup * romantic , data=student , subset=train )
TestError[6] = mean( ( student$G1 - predict( m6_train , student ) )[-train] ^2 )

TestError
plot( 1:6 , TestError , type="b" , xlab="model" , ylab="MSE" , main="Validation approach")

which.min( TestError )

  # shorten code a bit
set.seed(11111908) # use a different split
nStudents = dim(student)[1]
train = sample( nStudents , nStudents/2 )

 # collect all the model formulas in a vector
models = cbind( "G1 ~ 1" , "G1 ~ studytime" , "G1 ~ studytime + schoolsup" , "G1 ~ studytime * schoolsup" , "G1 ~ studytime * schoolsup + romantic" , "G1 ~ studytime * schoolsup * romantic" )

TestError = matrix( -9999 , 1 , length(models) ) # pre-allocation
for( tel in 1:length(models) )
{
  ResModel_train = glm( models[tel] , data=student , subset=train )
  TestError[tel] = mean( ( student$G1 - predict( ResModel_train , student ) )[-train] ^2 )
  rm( ResModel_train )
}

TestError

 # plot the test error (gives quite different results)
plot( 1:6 , TestError , type="b" , xlab="model" , ylab="MSE" , main="Validation approach")


  # do all in a function
  # validation approach (1 time)
source( "ComputeValidation.R" ) # put function in memory (repeat when changing code inside the function !)
ComputeValidation( 44453 , student , student$G1 , models )

  # validation approach (10 times, each times with a different split, to study variability in MSE estimate due to randomness of the split)
TestErrorAll = matrix( -9999 , 10 , 6 )
for( tel in 1:10 )
{
  TestErrorAll[ tel , ] = ComputeValidation( round(runif(1)*99999999) , student , student$G1 , models )
}
TestErrorAll

  # make a plot
  # to set the Y-axis limits (compute min and max of all values that have to be plotted)
minValue = min(TestErrorAll)
maxValue = max(TestErrorAll)

  # the first curve (for the first split) is plotted with 'plot'
plot( 1:6 , TestErrorAll[1,] , type="b" , xlab="models" , ylab="test error estimate" , main="validation approach" , sub="(increasing in complexity)" , ylim=c(minValue,maxValue) )
  # the other curvs (for split 2-10) are added to the plot with 'lines'
for( tel in 2:10 )
{
  lines( 1:6 , TestErrorAll[ tel , ] , type="b" )
}
  # plot also the means (over the 10 splits) by adding them with 'lines' (in red)
lines( 1:6 , apply(TestErrorAll,2,mean) , type="b" , col="red" , lwd=5 )

###########################################################################################################
###########################################################################################################

# LOOCV: select optimal model

models = cbind( "G1 ~ 1" , "G1 ~ studytime" , "G1 ~ studytime + schoolsup" , "G1 ~ studytime * schoolsup" , "G1 ~ studytime * schoolsup + romantic" , "G1 ~ studytime * schoolsup * romantic" )
LOOCV = matrix( -9999 , 1 , length(models) )
for( tel in 1:length(models) )
{
  print( tel ) # to follow the progress of the computations
  m = glm( models[tel] , data=student )
  m_loocv = cv.glm( student , m ) # when K is not specified, K=N (LOOCV)
  LOOCV[tel] = m_loocv$delta[1]
  rm( m , m_loocv )
}

plot( 1:6 , LOOCV , type="b" , xlab="models" , ylab="LOOCV test error estimate" , main="Leave-One-Out Cross-Validation" , sub="(increasing in complexity)" , col="red" , lwd=3 )

 # LOOCV with a function (I use the computational shortcut to compute LOOCV)
source( "ComputeLOOCV_short.R" )
LOOCV2 = ComputeLOOCV_short( student , models )
 
 # LOOCV should always give the same solution (here we check this)
LOOCV2
LOOCV
plot( 1:6 , LOOCV2 , type="b" , xlab="models" , ylab="LOOCV test error estimate" , main="Leave-One-Out Cross-Validation" , sub="(increasing in complexity)" , col="red" , lwd=3 )

###########################################################################################################
###########################################################################################################

# cross-validation: select optimal model

models = cbind( "G1 ~ 1" , "G1 ~ studytime" , "G1 ~ studytime + schoolsup" , "G1 ~ studytime * schoolsup" , "G1 ~ studytime * schoolsup + romantic" , "G1 ~ studytime * schoolsup * romantic" )
CV = matrix( -9999 , 1 , length(models) )
for( tel in 1:length(models) )
{
  print( tel )
  m = glm( models[tel] , data=student )
  m_cv = cv.glm( student , m , K=10 ) # K=10-fold CV
  CV[tel] = m_cv$delta[1] #the test error estimates is the first element in delta
  rm( m , m_cv )
}
CV

plot( 1:6 , CV , type="b" , xlab="models" , ylab="CV test error estimate" , main="Cross-Validation K=10" , sub="(increasing in complexity)" , col="red" , lwd=3 )

  # repeat 10-fold CV 10 times
source( "ComputeCV_repeated.R" )
models = cbind( "G1 ~ 1" , "G1 ~ studytime" , "G1 ~ studytime + schoolsup" , "G1 ~ studytime * schoolsup" , "G1 ~ studytime * schoolsup + romantic" , "G1 ~ studytime * schoolsup * romantic" )

 # check the input parameters to understand why the code should be like this
CV10_all = ComputeCV_repeated( student , models , 10 , 10 )

  # repeat 5-fold CV 10 times
CV5_all = ComputeCV_repeated( student , models , 5 , 10 )

  # repeat 2-fold CV 10 times
CV2_all = ComputeCV_repeated( student , models , 2 , 10 )

  # repeat 50-fold CV 10 times (ignore the warnings)
CV50_all = ComputeCV_repeated( student , models , 50 , 10 )

  # make a plot
  # to set the Y-axis limits (compute min and max of all values that have to be plotted)
minValue = min( c(CV10_all,CV5_all,CV2_all,CV50_all) )
maxValue = max( c(CV10_all,CV5_all,CV2_all,CV50_all) )

  # make a plot with 4 subplots in a 2x2 window
oldpar <- par()
par(mfrow=c(2,2))

 # plot for K=2 (the first CV is plotted with 'plot', the other 9 with 'lines', finally also the mean over the 10 cv's is plotted) )
plot( 1:6 , CV2_all[1,] , type="b" , xlab="models" , ylab="CV test error estimate" , main="Cross-validation K=2", sub="(increasing in complexity)" , ylim=c(minValue,maxValue) )
for( tel in 2:10 )
{
  lines( 1:6 , CV2_all[ tel , ] , type="b" )
}
lines( 1:6 , apply(CV2_all,2,mean) , type="b" , col="red" , lwd=2 ) #compute the mean over the 10 CV's

# plot for K=5
plot( 1:6 , CV5_all[1,] , type="b" , xlab="models" , ylab="CV test error estimate" , main="Cross-validation K=5", sub="(increasing in complexity)" , ylim=c(minValue,maxValue) )
for( tel in 2:10 )
{
  lines( 1:6 , CV5_all[ tel , ] , type="b" )
}
lines( 1:6 , apply(CV5_all,2,mean) , type="b" , col="red" , lwd=2 )

 # plot for K=10
plot( 1:6 , CV10_all[1,] , type="b" , xlab="models" , ylab="CV test error estimate" , main="Cross-validation K=10", sub="(increasing in complexity)" , ylim=c(minValue,maxValue) )
for( tel in 2:10 )
{
  lines( 1:6 , CV10_all[ tel , ] , type="b" )
}
lines( 1:6 , apply(CV10_all,2,mean) , type="b" , col="red" , lwd=2 )

# plot for K=50
plot( 1:6 , CV50_all[1,] , type="b" , xlab="models" , ylab="CV test error estimate" , main="Cross-validation K=50", sub="(increasing in complexity)" , ylim=c(minValue,maxValue) )
for( tel in 2:10 )
{
  lines( 1:6 , CV50_all[ tel , ] , type="b" )
}
lines( 1:6 , apply(CV50_all,2,mean) , type="b" , col="red" , lwd=2 )

 # to check that with K=50 the variance in the test error estimates (per model) is very small
CV50_all

 # you can use the CV-function also to perform LOOCV (K=N)
CVmax_all = ComputeCV_repeated( student , models , nStudents , 10 )
CVmax_all
LOOCV # to check that you get exactly the same result

###########################################################################################################
###########################################################################################################

# bootstrap (good for SE of parameter, not for determining test error !)

 # standard analysis (we can compare the SE's from the bootstrap with the SE's from the standard analysis)
m6 = glm( G1 ~ studytime * schoolsup * romantic , data=student )
summary(m6)

 # make a function that fits the model on the bootstrap sample, with index a vector of length N with the indices of the selected/sampled cases
regress.fn <- function( dataset , index )
{
  m = glm( G1 ~ studytime * schoolsup * romantic , data=dataset , subset=index )
  return( coef( m ) )
}

 # (as an example) generate 1 bootstrap sample and compute coefficients
nStudents = dim(student)[1]
set.seed( 47374627 )
regress.fn( student , sample( nStudents , nStudents , replace=T ) )

 # generate R=1000 bootstrap samples
boot( student , regress.fn , R=1000 )
summary(m6)$coef #compare with SE from standard analysis

###########################################################################################################
###########################################################################################################

## permutation test

# testing whether the correlation between G1 and studytime is 0
cor( student$G1 , student$studytime ) # observed correlation in the sample

nReplicates = 50000
PermutValues = matrix( -9999 , 1 , nReplicates )
nElem = dim(student)[1] #number of cases
for( tel in 1:nReplicates )
{
    # get a permutation from the values 1:nElem  
  permutation = sample( nElem , nElem , replace=F )
    # use the permutation of indices (1:nElem) to permute the values of studytime and compute the correlation with G1
  PermutValues[tel] = cor( student$G1 , student$studytime[permutation] )
  rm(permutation)
  
  # display the progression of the for-loop (after each 5000 iterations)
  if( (tel %% 5000) == 0 )
  {
    print(tel)
  }
}

oldpar <- par()
par(mfrow=c(1,1))
hist( PermutValues )

  # 2-sided p-value: how many bootstrap values are larger than .1583 or smaller than -.1583
p_value_twosided = mean( abs(PermutValues) > abs( cor( student$G1 , student$studytime ) ) )
p_value_twosided # p<.05: the observed correlation is significant

###########################################################################################################

# comparing the mean differences for two groups
# test whether the mean difference in G1 between people in a relation and those not in a relation differs significantly from zero
student$romantic
mean( student$G1[ student$romantic == "yes" ] )
mean( student$G1[ student$romantic == "no" ] )

# take into account that both groups can consist of a different number of people
Length_yes = length( student$G1[ student$romantic == "yes" ] )
Length_no = length( student$G1[ student$romantic == "no" ] )
Length_yes
Length_no

# compute the observed mean difference in the sample
mean_diff = mean( student$G1[ student$romantic == "no" ] ) - mean( student$G1[ student$romantic == "yes" ] )
mean_diff

nReplicates = 50000
PermutValues = matrix( -9999 , 1 , nReplicates )
nElem = dim(student)[1] #number of cases
for( tel in 1:nReplicates )
{
    # get a permutation from the values 1:nElem 
  PermutedOutcome = student$G1[ sample( nElem , nElem , replace=F ) ] 
    # put the first Length_no (permuted) cases in the no-group and the other in the yes-group
    # as such, cases are randomly distributed over the two groups (keeping the original group sizes, which are not the same here)
    # compute the mean difference for the permutation sample
  PermutValues[tel] = mean( PermutedOutcome[1:Length_no] ) - mean( PermutedOutcome[ (Length_no+1):nElem ] ) # attention: (Length_no+1) !!!
  rm(PermutedOutcome)
  
  # display the progression of the for-loop (after each 5000 iterations)
  if( (tel %% 5000) == 0 )
  {
    print(tel)
  }  
}

oldpar <- par()
par(mfrow=c(1,1))
hist( PermutValues )

 # two-sides p-value: is the mean difference equal to zero or not (H0 of no difference cannot be rejected)
p_value_twosided = mean( abs(PermutValues) > abs( mean_diff ) )
p_value_twosided

 # test that mean for 'no' is larger than mean for 'yes' (i.e., a positive mean difference)
 # now only look at mean differences < -.2728
p_value_onesided = mean( PermutValues < (mean_diff * -1) )
p_value_onesided

