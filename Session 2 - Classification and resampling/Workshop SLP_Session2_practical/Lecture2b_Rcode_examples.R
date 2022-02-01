# examples (chapter 4)
rm( list=ls() )
cat('\014')

library( ISLR )
library( MASS ) # LDA, QDA
library( e1071 ) # NB
library( class ) # KNN
library( boot )

#############################################################################################################
#############################################################################################################

student = read.csv2( "student-mat.csv" )
student$G1bin = student$G1>8 # to make a dichotomous outcome
table(student$G1bin) # check the number of cases in each class of the dichotomous outcome

# select the relevant variables from the data set
student = student[ c("G1","G1bin","studytime","schoolsup","romantic") ]

# logistic regression
nStudents = dim(student)[1]

# fit a logistic regression model (specify the binomial family)
m6_log = glm( G1bin ~ studytime * schoolsup * romantic , data=student , family=binomial )
summary(m6_log)

#remark: you can extract value from the summary-output (and use them in further computations)
summary(m6_log)$coef[,1] / summary(m6_log)$coef[,2]

# use the fitting regression coefficient to make a prediction for each case (based on its predictor values)
m6_log_probs = predict( m6_log , type="response" ) # contains the probability of a 1 on the criterion
m6_log_probs

# transform the above prediction in "FALSE" and "TRUE"
m6_log_pred = rep( "FALSE" , nStudents )
m6_log_pred[ m6_log_probs > .5 ] = "TRUE"
m6_log_pred

# make a confusion table (to see how many are classified correctly and how many incorrectly)
table( m6_log_pred , student$G1bin )

# for the training data: compute the rate of correct classification (i.e., sum of diagonal of confusion matrix)
mean( m6_log_pred == student$G1bin )

# for the training data: compute the misclassification rate ( i.e., sum of off-diagonal elements of confusion matrix)
1 - mean( m6_log_pred == student$G1bin )

#############################################################################################################
#############################################################################################################

### LINEAR DISCRIMINANT ANALYSIS

View(Smarket)
pairs( Smarket[2:3], main = "Smarket data (red=Down,green=Up)", pch = 21, bg = c("red", "green")[unclass(Smarket$Direction)])

 # training is period for 2005
lda_fit = lda( Direction ~ Lag1 + Lag2 , data = Smarket , subset=Year<2005 )
lda_fit

 # plot of linear discriminant values for each group
plot( lda_fit )

 # predict 2005 (you only use period before it to train)
Smarket_2005 = subset( Smarket , Year==2005 ) #make the validation set (only measurements from 2005)
lda_pred = predict( lda_fit , Smarket_2005 ) #do predictions

 # we want to see the predictions, but what type of object is lda_pred?
lda_pred[ 1:5 , ]
class(lda_pred)
str(lda_pred)
data.frame( lda_pred )[ 1:5 , ]

 # confusion matrix
table( lda_pred$class , Smarket_2005$Direction )

 # correct classification rate
mean( lda_pred$class == Smarket_2005$Direction )

 # error (misclassification) rate
1 - mean( lda_pred$class == Smarket_2005$Direction )


### QUADRATIC DISCRIMINANT ANALYSIS
qda_fit = qda( Direction ~ Lag1 + Lag2 , data = Smarket , subset=Year<2005 ) # fit on training
qda_pred = predict( qda_fit , Smarket_2005 ) # predictions for validation set

 # confusion matrix
table( qda_pred$class , Smarket_2005$Direction )

 # correct classification rate
mean( qda_pred$class == Smarket_2005$Direction )

 # misclassification rate
1 - mean( qda_pred$class == Smarket_2005$Direction )


### NAIVE BAYES RULE

#library(e1071)
nab_fit = naiveBayes( Direction ~ Lag1 + Lag2 , data = Smarket , subset=Year<2005 ) # fit on training
nab_pred = predict( nab_fit , Smarket_2005 ) # predict for validation set

 # confusion matrix
table( nab_pred , Smarket_2005$Direction )

 # correct classification rate
mean( nab_pred == Smarket_2005$Direction )

 # misclassification rate
1 - mean( nab_pred == Smarket_2005$Direction )


#############################################################################################################
#############################################################################################################

### K-NEAREST NEIGHBOR

#require( class )

 # make a matrix with the predictors as columns
Xlag = cbind( Smarket$Lag1 , Smarket$Lag2 )

 # draw a training sample
train = Smarket$Year < 2005

 # fit knn
set.seed( 10 )
knn_pred = knn( Xlag[train,] , Xlag[!train,] , Smarket$Direction[train] , k=1 )

 # confusion matrix
table( knn_pred , Smarket$Direction[!train] )

 # correct classification rate
mean( knn_pred == Smarket$Direction[!train] )

 # misclassification rate
1 - mean( knn_pred == Smarket$Direction[!train] )

## try other values for k
nNeighbors = 20
set.seed( 10 )
knn_ClassifRate = matrix( -9999 , 1 , nNeighbors ) #pre-allocation
for( tel in 1:nNeighbors )
{
  knn_pred = knn( Xlag[train,] , Xlag[!train,] , Smarket$Direction[train] , k=tel )
  knn_ClassifRate[tel] = mean( knn_pred == Smarket$Direction[!train] )
}
plot( 1:nNeighbors , 1-knn_ClassifRate , type="b" , xlab="k" , ylab="misclassification rate" , main="K-nearest neighbors" )

