#determine optimal model for KNN

# make a matrix with the predictors as columns (you need to do this 4 times)
# model 1: G1bin ~ studytime + failures
# model 2: G1bin ~ studytime * failures
# model 3: G1bin ~ studytime * failures + absences
# model 4: G1bin ~ studytime * failures * absences
frfa = student$freetime * student$failures
frab = student$freetime * student$absences
faab = student$failures * student$absences
frfaab = student$freetime * student$failures * student$absences

X1 = cbind( student$freetime , student$failures )
Xtrain1 = X1[TrainingCases,]
Xtest1 = X1[TestCases,]

X2 = cbind( student$freetime , student$failures , frfa )
Xtrain2 = X2[TrainingCases,]
Xtest2 = X2[TestCases,]

X3 = cbind( student$freetime , student$failures , frfa , student$absences )
Xtrain3 = X3[TrainingCases,]
Xtest3 = X3[TestCases,]

X4 = cbind( student$freetime , student$failures , frfa , student$absences , frab , faab , frfaab )
Xtrain4 = X4[TrainingCases,]
Xtest4 = X4[TestCases,]

Ytrain = student$G1bin[ TrainingCases ]
Ytest = student$G1bin[ TestCases ]

source( "knn_CV.R" )
cv_errors_KNN = matrix( -9999 , nFolds , nModels )
for( fold in 1:nFolds )
{
  optK = knn_CV( Xtrain1[CVfolds!=fold,] , Ytrain[CVfolds!=fold] , MaxK=50 , nFolds=10 )    
  knn_pred = knn( Xtrain1[CVfolds!=fold,] , Xtrain1[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=optK )
  cv_errors_KNN[ fold , 1 ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
  
  optK = knn_CV( Xtrain2[CVfolds!=fold,] , Ytrain[CVfolds!=fold] , MaxK=50 , nFolds=10 )
  knn_pred = knn( Xtrain2[CVfolds!=fold,] , Xtrain2[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=optK )
  cv_errors_KNN[ fold , 2 ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
  
  optK = knn_CV( Xtrain3[CVfolds!=fold,] , Ytrain[CVfolds!=fold] , MaxK=50 , nFolds=10 )
  knn_pred = knn( Xtrain3[CVfolds!=fold,] , Xtrain3[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=optK )
  cv_errors_KNN[ fold , 3 ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
  
  optK = knn_CV( Xtrain4[CVfolds!=fold,] , Ytrain[CVfolds!=fold] , MaxK=50 , nFolds=10 )
  knn_pred = knn( Xtrain4[CVfolds!=fold,] , Xtrain4[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=optK )
  cv_errors_KNN[ fold , 4 ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
}

mean_cv_errors_KNN = apply( cv_errors_KNN , 2 , mean )
plot( 1:nModels , mean_cv_errors_KNN , type="b" , xlab="model" , ylab="misclassification rate" , main="KNN" )
optimalModel_KNN = which.min( mean_cv_errors_KNN )

# step 2: determine test error for the optimal KNN model (with optimal K)
