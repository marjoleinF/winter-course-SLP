# exercises (chapter 4)
rm( list = ls() )
cat('\014')

library( ISLR )
library( leaps ) #BSS
library( glmnet ) #regularization

student_full = read.csv2( "student-mat.csv" )
View(student_full)
nStudents = nrow(student_full)

#############################################################################################################
#############################################################################################################
############## QUESTION 1 (ridge and lasso regression)

 # make matrix with predictors (in the columns)
x = model.matrix( G3 ~ .-1 , data=student_full )

# call outcome vector y
y = student_full$G3

# 10-fold cross-validation
cv_ridge = cv.glmnet( x , y , alpha=0 ) #default is 10-fold (nfolds)
cv_lasso = cv.glmnet( x, y ) #alpha = 1 (lasso) is default

par(mfrow=c(1,2))
plot( cv_ridge )
plot( cv_lasso )

cv_ridge$lambda.min #optimal lambda for ridge
cv_ridge$lambda.1se #optimal lambda for ridge (using the 1SE rule)
coef( cv_ridge , s=cv_ridge$lambda.1se ) #get coefficients for model with best lambda

cv_lasso$lambda.min #optimal lambda for lasso
cv_lasso$lambda.1se #optimal lambda for lasso (using the 1SE rule)
coef( cv_lasso , s=cv_lasso$lambda.1se ) #get coefficients for model with best lambda

##########################################################################################################
##########################################################################################################
############## QUESTION 2 (ridge and lasso logistic regression)

 # logistic regression
student_full$G3bin = student_full$G3>8 # to make a dichotomous outcome

x = model.matrix( G3bin ~ .-1 , data=student_full )
y = student_full$G3bin

nFolds = 10
set.seed( 500 )
CVfolds = sample( rep( 1:nFolds , length=nStudents ) , nStudents ) #make a vector of 1...10 and permute it
cv_errors_ridge = matrix( -9999 , 1 , nFolds )
cv_errors_lasso = matrix( -9999 , 1 , nFolds )

nFolds_inner = 10
for( fold in 1:nFolds )
{
  xtrain = x[ CVfolds!=fold , ]
  ytrain = y[ CVfolds!=fold ]
  xtest = x[ CVfolds==fold , ]
  ytest = y[ CVfolds==fold ]
  
  cv_ridge = cv.glmnet( xtrain , ytrain , alpha=0 , nfolds = nFolds_inner ) #10-fold CV on training data from each (outer)fold
  cv_ridge_train = glmnet( xtrain , ytrain , alpha=0 , lambda=cv_ridge$lambda.min )
  predict_ridge_train = predict( cv_ridge_train , xtest )
  cv_errors_ridge[ fold ] = sqrt( apply( ( ytest - predict_ridge_train )^2 , 2 , mean ) )
  
  cv_lasso = cv.glmnet( xtrain , ytrain , alpha=1 , nfolds = nFolds_inner ) #10-fold CV on training data from each (outer)fold
  cv_lasso_train = glmnet( xtrain , ytrain , alpha=1 , lambda=cv_lasso$lambda.min )
  predict_lasso_train = predict( cv_lasso_train , xtest )
  cv_errors_lasso[ fold ] = sqrt( apply( ( ytest - predict_lasso_train )^2 , 2 , mean ) )    
}

mean( cv_errors_ridge )
mean( cv_errors_lasso )

#to determine the most important predictors for G3bin
#take the best model (here: lasso), do analyse on full data, with 10-fold CV to determine optimal lambda)
coef( cv.glmnet( x , y , alpha=1 , nfolds = nFolds ) )