# examples (chapter 6.3)
rm( list=ls() )
cat('\014')

library( ISLR )
library( pls )

 # remove missing values
Hitters = na.omit( Hitters )

x = model.matrix( Salary ~ . , Hitters )[,-1]  #remove intercept
y = Hitters$Salary

set.seed( 100 )
train = sample( 1:nrow(x) , nrow(x)/2 )
test = (-train)
x.test = x[test,]
y.test = y[test]

#############################################################################################################

##Principal Components Regression (PCR)

  #perform 10-fold cross-validation (on training data) to determine the number of components and estimate the test error (on validation data)
set.seed( 9999 )
pcr_fit_train = pcr( Salary ~ . , data = Hitters , subset=train , scale=TRUE , validation="CV" )
summary( pcr_fit_train )
validationplot( pcr_fit_train , val.type="MSEP" )

 #estimate test MSE
pcr_predict = predict( pcr_fit_train , x[ test , ] , ncomp=6 )
pcr_mse = mean( ( pcr_predict - y.test ) ^2 )
pcr_mse

 #fit PCR with 6 components to full data
pcr_fit_full = pcr( y ~ x , scale=TRUE , ncomp=6 )
summary( pcr_fit_full )

#############################################################################################################
#############################################################################################################

##Partial Least Squares (PLS) regression

  #perform 10-fold cross-validation (on training data) to determine the number of components and estimate the test error (on validation data)
set.seed( 500 )
pls_fit_train = plsr( Salary ~ . , data = Hitters , subset=train , scale=TRUE , validation="CV" )
summary( pls_fit_train )
validationplot( pls_fit_train , val.type="MSEP" )

 #estimate test MSE
pls_predict = predict( pls_fit_train , x[ test , ] , ncomp=13 )
pls_mse = mean( ( pls_predict - y.test ) ^2 )
pls_mse

  #fit PCR with 2 components to full data
pls_fit_full = plsr( y ~ x , scale=TRUE , ncomp=13 )
summary( pls_fit_full )

#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################

# examples (chapter 10)
rm( list=ls() )

library( ISLR )

dim( USArrests )
dimnames( USArrests )
row.names( USArrests )
names( USArrests )

#############################################################################################################

##Principal Component Analysis (PCA)
pca_result = prcomp( USArrests , scale=TRUE ) # centered (default) and normalized variables
names( pca_result )

pca_result$rotation # component loadings
pca_result$x # component scores

 # explained variance per component
pca_result$sdev # standard deviations of the component scores
sqrt( apply( pca_result$x , 2 , var ) ) # to check: compute the standard deviation of the component scores

var_expl = pca_result$sdev ^ 2 # variance explained per component
prop_var_expl = var_expl / sum(var_expl) 
prop_var_expl # proportion variance explained per component
cumul_prop_var_expl = cumsum( prop_var_expl ) # cumulative proportion variance explained (for 1, 2, ... components)
cumul_prop_var_expl

summary( pca_result ) # gives output for the variance explained
plot( pca_result ) # plots the variance explained

 # scree plot
par(mfrow=c(1,2))
plot( prop_var_expl , type="b" , xlab="component number" , ylab="proportion variance explained" , ylim=c(0,1) )
plot( cumul_prop_var_expl , type="b" , xlab="number of components" , ylab="cumulative prop. var. expl." , ylim=c(0,1) )

 # some way to get an idea of the elbow in the scree plot
nElem = length( cumul_prop_var_expl )
ratios = ( cumul_prop_var_expl[2:(nElem-1)] - cumul_prop_var_expl[1:(nElem-2)] ) / ( cumul_prop_var_expl[3:nElem] - cumul_prop_var_expl[2:(nElem-1)] )
ratios
nComponents_selected = which.max( ratios ) + 1
nComponents_selected

 # biplot (first two principal components)
par( mfrow=c(1,1) )
biplot( pca_result , scale=0 , cex=.6 ) #cex to change fond of the figure
 # mirror the variables in the biplot
pca_result2 = pca_result
pca_result2$rotation = -pca_result$rotation
pca_result2$x = -pca_result$x
biplot( pca_result2 , scale=0 , cex=.6 )