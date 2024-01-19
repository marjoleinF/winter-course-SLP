# examples (chapter 6)

rm(list = ls())
cat('\014')

library( ISLR )
library( leaps ) # BSS
library( glmnet ) # ridge regression and lasso

#############################################################################################################
#############################################################################################################

View(Hitters) #baseball data base (predict salary of baseball players)
head(Hitters)
summary(Hitters)
dim(Hitters) # check the number of cases
with( Hitters , sum(is.na(Salary)) ) #check whether there are missing values in the Salary-variable

 # remove incomplete cases (missing values)
Hitters = na.omit(Hitters)
dim(Hitters) #check the number of cases after removal
with( Hitters , sum(is.na(Salary)) ) #check whether missing values are removed

### best subset selection (BSS; on full data)
model_BSS = regsubsets( Salary ~ . , data = Hitters ) #default is subsets of 8 predictors
summary( model_BSS )

model_BSS = regsubsets( Salary ~ . , data = Hitters , nvmax = 19 ) #go up to subsets of 19 predictors
summary_BSS = summary( model_BSS )
 #to see what information is in summary_BSS
names( summary_BSS )
str( summary_BSS )

 # plot Mallow's Cp values
plot( summary_BSS$cp , xlab="Number of variables" , ylab="Cp" )
which.min( summary_BSS$cp ) # gives index of smallest number in a vector
points( 10 , summary_BSS$cp[10] , pch=20 , col="red" ) # indicate optimal solution in the plot

plot( model_BSS , scale="Cp" ) #also possible for bic, r2, adjr2
coef( model_BSS , 10 )

###########################################################################################################
###########################################################################################################

 # Forward stepwise selection (on full data)
model_FSS = regsubsets( Salary ~ . , data = Hitters , nvmax = 19 , method="forward" )
summary( model_FSS )
plot( model_FSS , scale="Cp" )

##########################################################

  # determine complexity of the model
  # train and validation set
set.seed( 100 )
dim(Hitters)[1]
train = sample( seq(263) , 180 , replace=FALSE ) # 2/3 is training set
train

 # fit model on training set
model_FSS = regsubsets( Salary ~ . , data = Hitters[train,] , nvmax = 19 , method="forward" )

  #possibility 1 to get test error
val_errors = rep( NA , 19 ) # pre-allocation: you can also do this with matrix- or vector-function
x_test = model.matrix( Salary ~ . , data = Hitters[-train,] ) # make the validation set
for( tel in 1:19 )
{
  coefi = coef( model_FSS , id=tel ) # extract coefficient for model of size 'tel'
  pred = x_test[ , names(coefi) ] %*% coefi # compute predictions
  val_errors[ tel ] = mean( ( Hitters$Salary[-train] - pred ) ^2 ) # compute RSS (test set error)
}

  #possibility 2 to get test error
# make a predict-function for subset regression
predict.regsubsets <- function( object , newdata , id , ... ) #object is fitted training data
{
  formula = as.formula( object$call[[2]] ) #extract the formula (is second element of call)
  mat = model.matrix( formula , newdata ) #make model matrix using the formula
  coefi = coef( object , id=id ) #extract coefficients for model with id predictors
  mat[ , names(coefi) ] %*% coefi #make predictions
}

val_errors2 = rep( NA , 19 )
for( tel in 1:19 )
{
  pred = predict( model_FSS , Hitters[ -train , ] , id=tel )
  val_errors2[ tel ] = mean( ( Hitters$Salary[ -train ] - pred ) ^2 )
}
val_errors
val_errors2

 # to set the limits of the Y-axis of the plot
par(mfrow=c(1,1))
allvalues = c( sqrt(val_errors) , sqrt( model_FSS$rss[-1] / 180 ) )
plot( sqrt( val_errors ) , type="b" , ylab="root MSE" , ylim=c(min(allvalues),max(allvalues)) , pch=19 )
points( sqrt( model_FSS$rss[-1] / 180 ) , type="b" , pch=19 , col="blue" ) # plot training error but not for the null model (-1)
legend( "topright" , legend=c("Training","validation") , col=c("blue","black") , pch=19 ) #pch=19 is a solid dot

##################################################

  # other way of determining model complexity
  # 10-fold cross-validation
set.seed( 500 )
folds = sample( rep( 1:10 , length=nrow(Hitters) ) ) #make a vector of 1...10 and permute it
folds
table( folds ) # to check whether you have equal amount of 1, 2, ... 's

cv_errors = matrix( -9999 , 10 , 19 ) #10 folds and 19 subsets
for( k in 1:10 )
{
   # fit on training data (for fold k)
   # NOTE: the selection of the variables is part of the cross-validation (for each fold,
   #       you may end up with different variables being selected )
  best_fit = regsubsets( Salary ~ . , data=Hitters[ folds!=k , ] , nvmax=19 , method="forward" )
  for( tel in 1:19 )
  {
     # predict for validation data (for fold k)
    pred = predict( best_fit , Hitters[ folds==k , ] , id=tel )
     # compute RSS
    cv_errors[ k , tel ] = mean( ( Hitters$Salary[ folds==k ] - pred ) ^2 )
  }
}

rmse_cv = sqrt( apply( cv_errors , 2 , mean ) ) #average RMSE over 10 folds (i.e., compute column means)
plot( rmse_cv , type="b" , pch=19 )

##########################################""

### backward stepwise selection
model_BSS = regsubsets( Salary ~ . , data = Hitters , nvmax = 19 , method="backward" )
summary( model_BSS )
plot( model_BSS , scale="Cp" )

##########################################################################################################
##########################################################################################################

### PENALISATION METHODS

# ridge regression

 # make matrix with predictors (in the columns)
x = model.matrix( Salary ~ .-1 , data=Hitters )
 # call outcome vector y
y = Hitters$Salary

 # ridge regression (on full data)
fit_ridge = glmnet( x , y , alpha=0 , standardize=TRUE ) #you can specify values for lambda: lambda=grid 10^seq(10,-2,length=100)
plot( fit_ridge , xvar="lambda" , label=TRUE ) # plot coefficients as a function of lambda

 # 10-fold cross-validation to determine lambda
cv_ridge = cv.glmnet( x , y , alpha=0 ) #default is 10-fold (nfolds)
plot( cv_ridge )
coef( cv_ridge )

#####################################################

 # lasso (default value for lambda in glmnet-call)
fit_lasso = glmnet( x , y )
plot( fit_lasso , xvar="lambda" , label=TRUE ) # plot coefficients as a function of lambda
plot( fit_lasso , xvar="dev" , label=TRUE ) # plot coefficients as a function of deviance explained (R-squared)

 # 10-fold cross-validation to determine lambda
cv_lasso = cv.glmnet( x, y ) #alpha = 1 is default
plot( cv_lasso )
coef( cv_lasso )

#####################################################

 # determine best lambda for lasso (using the validation set approach)
lasso_train = glmnet( x[ train , ] , y[train] ) # fit model on training set
lasso_train

 # predictions for the validation set
lasso_predict = predict( lasso_train , x[ -train , ] ) #s=lambda-value (you can specify for which lambda values predictions should be made)
dim( lasso_predict )
lasso_predict[ 1:10 , 1:5 ]
rmse_lasso = sqrt( apply( ( y[-train] - lasso_predict )^2 , 2 , mean ) ) # compute RSS

plot( log(lasso_train$lambda) , rmse_lasso , type="b" , xlab="log(lambda)" )
best_lambda = lasso_train$lambda[ order(rmse_lasso)[1] ] #find lambda for best model
best_lambda
coef( lasso_train , s=best_lambda ) # get coefficients for model with best lambda

#####################################################

# elastic net (other penalty is used)
fit_en = glmnet( x , y , alpha=.6 )
plot( fit_en , xvar="lambda" , label=TRUE )