# exercises (chapter 4)
rm( list = ls() )
cat('\014')

library( ISLR )
library( MASS )
library( e1071 ) # NB
library( class ) # KNN
library( boot ) # cv.glm

student_full = read.csv2( "student-mat.csv" )
student_full$G1bin = factor( student_full$G1>8 ) # to make a dichotomous outcome

# select the relevant variables from the data set
student = student_full[ c("G1","G1bin","studytime","absences","freetime","failures","age") ]
View(student)

#############################################################################################################
#############################################################################################################
######## QUESTION 1 ##########

set.seed(542156)

# LOGISTIC REGRESSION

 # 10-fold cross-validation of logistic regression model
m = glm( G1bin ~ studytime + absences + freetime + failures + age , data=student , family=binomial )
m_cv = cv.glm( student , m , K=10 )
str(m_cv)
m_cv$delta[1] # is now a misclassification rate (and not a mean squared error !!!)

#############################################################################################################
#############################################################################################################
######## QUESTION 2 ##########

### LINEAR DISCRIMINANT ANALYSIS

 # fit model on training set
 # I adapted the use of the sample function to make equal split (50-50)
TrainingCases = sample( rep(c(TRUE,FALSE),length=nrow(student)) , nrow(student) , rep=FALSE )
table(TrainingCases)
lda_fit = lda( G1bin ~ studytime + absences + freetime + failures + age , data = student , subset = TrainingCases )

 # make predictions for test set based on model from training data
testset = subset( student , !TrainingCases )
lda_pred = predict( lda_fit , testset , type="response" ) # predictions for validation data

 # confusion table
table( lda_pred$class , testset$G1bin )

 # correct classification rate
mean( lda_pred$class == testset$G1bin )

 # misclassification rate
1 - mean( lda_pred$class == testset$G1bin )

##################################################

### QUADRATIC DISCRIMINANT ANALYSIS
 # fit on training data
qda_fit = qda( G1bin ~ studytime + absences + freetime + failures + age , data = student , subset=TrainingCases )
qda_pred = predict( qda_fit , testset , type="response" ) # predictions for validation data

 # confusion matrix
table( qda_pred$class , testset$G1bin )

 # correct classification rate
mean( qda_pred$class == testset$G1bin )

 # misclassification rate
1 - mean( qda_pred$class == testset$G1bin )

##################################################

### NAIVE BAYES RULE

nab_fit = naiveBayes( G1bin ~ studytime + absences + freetime + failures + age , data = student , subset = TrainingCases ) #fit on training data
nab_pred = predict( nab_fit , testset ) #predictions for validation data

 # confusion matrix
table( nab_pred , testset$G1bin )
 # for some unknown reason, naive Bayes does not give results here

 # correct classification rate
mean( nab_pred == testset$G1bin )

 # misclassification rate
1 - mean( nab_pred == testset$G1bin )

#############################################################################################################
#############################################################################################################
######## QUESTION 3 ##########

# K-NEAREST NEIGHBOR
#2 steps needed
#  step 1) determine optimal K by 10-fold CV on training set
#  step 2) determine the test error (using the test set) for the model with optimal K

set.seed( 10 )
nStudents = nrow(student)
TestCases = sample( nStudents , nStudents/5 ) #take 20% of data as test set (to determine test error in step 2)
TrainingCases = 1:nStudents
TrainingCases = TrainingCases[-TestCases]
nTrainingCases = length(TrainingCases)

 # make a matrix with the predictors as columns
X = cbind( student$studytime , student$absences , student$freetime , student$failures , student$age )
Xtrain = X[TrainingCases,]
Xtest = X[TestCases,]
Ytrain = student$G1bin[ TrainingCases ]
Ytest = student$G1bin[ TestCases ]

 #determine CV folds for the training data
CVfolds = sample( rep(1:10,length=nTrainingCases) , nTrainingCases )

nK = 50
nFolds = 10
cv_errors = matrix( -9999 , nFolds , nK ) #10 folds and nK values for K
for( fold in 1:nFolds )
{
  for( Ktel in 1:nK )
  {
    knn_pred = knn( Xtrain[CVfolds!=fold,] , Xtrain[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=Ktel )
    cv_errors[ fold , Ktel ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
  }
}

 # compute mean classification rate across 10 folds (and make a plot and determine the optimal K)
mean_cv_errors = apply( cv_errors , 2 , mean )
plot( 1:nK , mean_cv_errors , type="b" , xlab="k" , ylab="misclassification rate" , main="K-nearest neighbors" )
optimalK = which.min( mean_cv_errors )

 # step 2: determine test error for the model with optimal K
 #NOTE: instead of using the k with lowest MSE, you can also apply some standard error or %-rule to select a more parsimonius model
knn_pred = knn( Xtrain , Xtest , Ytrain , k=optimalK )
1 - mean( knn_pred == Ytest )

#############################################################################################################
#############################################################################################################
######## QUESTION 4 ##########

#choose 4 nested models
# model 1: G1bin ~ studytime + failures
# model 2: G1bin ~ studytime * failures
# model 3: G1bin ~ studytime * failures + absences
# model 4: G1bin ~ studytime * failures * absences
models = cbind( "G1bin ~ studytime + failures" , "G1bin ~ studytime * failures" , "G1bin ~ studytime * failures + absences" , "G1bin ~ studytime * failures * absences" )
nModels = length( models ) #how much models are there in the comparison?

#2 steps
#  step 1: determine the optimal model (amongst the four nested models) with 10-fold CV
#  step 2: determine the test error for the optimal model

set.seed( 945287 )
nStudents = nrow(student)
TestCases = sample( nStudents , nStudents/5 ) #take 20% of data as test set (to determine test error in step 2)
TrainingCases = 1:nStudents
TrainingCases = TrainingCases[-TestCases]
nTrainingCases = length(TrainingCases)
nTestCases = length(TestCases)
student_train = student[ TrainingCases , ]
student_test = student[ TestCases , ]

#determine CV folds for the training data
nFolds = 10

#determine optimal model for logistic regression
mean_cv_errors_logreg = matrix( -9999 , 1 , nModels )
for( modtel in 1:nModels )
{
  m = glm( models[modtel] , data=student_train , family=binomial )
  m_cv = cv.glm( student_train , m , K=nFolds )
  mean_cv_errors_logreg[ modtel ] = m_cv$delta[1]
}
plot( 1:nModels , mean_cv_errors_logreg , type="b" , xlab="model" , ylab="misclassification rate" , main="logistic regression" )
optimalModel_logreg = which.min( mean_cv_errors_logreg )

# step 2: determine test error for the optimal logistic regression model
mopt_logreg = glm( models[optimalModel_logreg] , data=student_train , family=binomial )
mopt_logreg_probs = predict( mopt_logreg , newdata=student_test , type="response" ) 
mopt_logreg_pred = rep( "FALSE" , nTestCases )
mopt_logreg_pred[ mopt_logreg_probs > .5 ] = "TRUE"
table( mopt_logreg_pred , student_test$G1bin )
mean( mopt_logreg_pred == student_test$G1bin )
1 - mean( mopt_logreg_pred == student_test$G1bin )

#########################################################

#determine optimal model for LDA and QDA
CVfolds = sample( rep(1:nFolds,length=nTrainingCases) , nTrainingCases )
cv_errors_LDA = matrix( -9999 , nFolds , nModels ) #10 folds and nModels to compare
cv_errors_QDA = matrix( -9999 , nFolds , nModels ) #10 folds and nModels to compare
for( fold in 1:nFolds )
{
  trainingfold = (CVfolds!=fold)
  #LDA
  lda_fit = lda( G1bin ~ studytime + failures , data=student_train , subset=trainingfold )
  lda_pred = predict( lda_fit , newdata=student_train[ CVfolds==fold , ] , type="response" )
  cv_errors_LDA[ fold , 1 ] = 1 - mean( lda_pred$class == student_train$G1bin[CVfolds==fold] )
  
  lda_fit = lda( G1bin ~ studytime * failures , data=student_train , subset=trainingfold )
  lda_pred = predict( lda_fit , newdata=student_train[ CVfolds==fold , ] , type="response" )
  cv_errors_LDA[ fold , 2 ] = 1 - mean( lda_pred$class == student_train$G1bin[CVfolds==fold] )
  
  lda_fit = lda( G1bin ~ studytime * failures + absences , data=student_train , subset=trainingfold )
  lda_pred = predict( lda_fit , newdata=student_train[ CVfolds==fold , ] , type="response" )
  cv_errors_LDA[ fold , 3 ] = 1 - mean( lda_pred$class == student_train$G1bin[CVfolds==fold] )
  
  lda_fit = lda( G1bin ~ studytime * failures * absences , data=student_train , subset=trainingfold )
  lda_pred = predict( lda_fit , newdata=student_train[ CVfolds==fold , ] , type="response" )
  cv_errors_LDA[ fold , 4 ] = 1 - mean( lda_pred$class == student_train$G1bin[CVfolds==fold] )
  
  #QDA
  qda_fit = qda( G1bin ~ studytime + failures , data=student_train , subset=trainingfold )
  qda_pred = predict( qda_fit , newdata=student_train[ CVfolds==fold , ] , type="response" )
  cv_errors_QDA[ fold , 1 ] = 1 - mean( qda_pred$class == student_train$G1bin[CVfolds==fold] )
  
  qda_fit = qda( G1bin ~ studytime * failures , data=student_train , subset=trainingfold )
  qda_pred = predict( qda_fit , newdata=student_train[ CVfolds==fold , ] , type="response" )
  cv_errors_QDA[ fold , 2 ] = 1 - mean( qda_pred$class == student_train$G1bin[CVfolds==fold] )
  
  qda_fit = qda( G1bin ~ studytime * failures + absences , data=student_train , subset=trainingfold )
  qda_pred = predict( qda_fit , newdata=student_train[ CVfolds==fold , ] , type="response" )
  cv_errors_QDA[ fold , 3 ] = 1 - mean( qda_pred$class == student_train$G1bin[CVfolds==fold] )
  
  qda_fit = qda( G1bin ~ studytime * failures * absences , data=student_train , subset=trainingfold )
  qda_pred = predict( qda_fit , newdata=student_train[ CVfolds==fold , ] , type="response" )
  cv_errors_QDA[ fold , 4 ] = 1 - mean( qda_pred$class == student_train$G1bin[CVfolds==fold] )
}

mean_cv_errors_LDA = apply( cv_errors_LDA , 2 , mean )
plot( 1:nModels , mean_cv_errors_LDA , type="b" , xlab="model" , ylab="misclassification rate" , main="LDA" )
optimalModel_LDA = which.min( mean_cv_errors_LDA )
optimalModel_LDA

mean_cv_errors_QDA = apply( cv_errors_QDA , 2 , mean )
plot( 1:nModels , mean_cv_errors_QDA , type="b" , xlab="model" , ylab="misclassification rate" , main="QDA" )
optimalModel_QDA = which.min( mean_cv_errors_QDA )
optimalModel_QDA

# step 2: determine test error for the optimal LDA and QDA
mopt_LDA = lda( G1bin ~ studytime * failures , data = student_train )
mopt_LDA_pred = predict( mopt_LDA , newdata=student_test , type="response" )
table( mopt_LDA_pred$class , student_test$G1bin )
mean( mopt_LDA_pred$class == student_test$G1bin )
1 - mean( mopt_LDA_pred$class == student_test$G1bin )

mopt_QDA = qda( G1bin ~ studytime + failures , data = student_train )
mopt_QDA_pred = predict( mopt_QDA , newdata=student_test , type="response" )
table( mopt_QDA_pred$class , student_test$G1bin )
mean( mopt_QDA_pred$class == student_test$G1bin )
1 - mean( mopt_QDA_pred$class == student_test$G1bin )

#########################################################

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

X2 = cbind( student$freetime , student$failures , frfa )
Xtrain2 = X2[TrainingCases,]

X3 = cbind( student$freetime , student$failures , frfa , student$absences )
Xtrain3 = X3[TrainingCases,]

X4 = cbind( student$freetime , student$failures , frfa , student$absences , frab , faab , frfaab )
Xtrain4 = X4[TrainingCases,]

Ytrain = student$G1bin[ TrainingCases ]
Ytest = student$G1bin[ TestCases ]

nK = 50
cv_errors_KNN = array( -9999 , c(nFolds , nModels , nK ) )
for( fold in 1:nFolds )
{
  for( ktel in 1:nK )
  {
    knn_pred = knn( Xtrain1[CVfolds!=fold,] , Xtrain1[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=ktel )
    cv_errors_KNN[ fold , 1 , ktel ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
    
    knn_pred = knn( Xtrain2[CVfolds!=fold,] , Xtrain2[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=ktel )
    cv_errors_KNN[ fold , 2 , ktel ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
    
    knn_pred = knn( Xtrain3[CVfolds!=fold,] , Xtrain3[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=ktel )
    cv_errors_KNN[ fold , 3 , ktel ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
    
    knn_pred = knn( Xtrain4[CVfolds!=fold,] , Xtrain4[CVfolds==fold,] , Ytrain[CVfolds!=fold] , k=ktel )
    cv_errors_KNN[ fold , 4 , ktel ] = 1 - mean( knn_pred == Ytrain[ CVfolds==fold ] )
  }
}

mean_cv_errors_KNN = apply( cv_errors_KNN , c(2,3) , mean )

#find out which model in combination with which k has the lowest test error
best_model = which( mean_cv_errors_KNN == min(mean_cv_errors_KNN) , arr.ind = TRUE )

#model 1 with several K-values (15-20) is optimal
#take the largest K-value (the less flexible model = more parsimonious)
optK = best_model[1,2]

# step 2: determine test error for the optimal KNN model (with optimal K)
Xopt = cbind( student$freetime , student$failures )
Xtrain = Xopt[TrainingCases,]
Xtest = Xopt[TestCases,]

knn_pred = knn( Xtrain , Xtest , Ytrain , k=optK )
1 - mean( knn_pred == Ytest )