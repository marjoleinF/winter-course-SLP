rm( list=ls() )
cat('\014')

library( ISLR )

#remove variable 2 (Apps): is the dependent variable
#remvoe variable 1 (Private)
data_College = College[,-c(1,2)]

 # PCA

pca_result = prcomp( data_College , scale=TRUE ) # centered (default) and normalized variables

 # explained variance per component
summary( pca_result ) # gives output for the variance explained
plot( pca_result ) # plots the variance explained

prop_var_expl = summary( pca_result )$importance[2,]
cumul_prop_var_expl = summary( pca_result )$importance[3,]

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

# interpretation
round( pca_result$rotation[ , 1:2] , 2 )# component loadings
round( pca_result$x[ , 1:2] , 2 )# component scores

# biplot (first two principal components)
par(mfrow=c(1,1))
biplot( pca_result , scale=0 , cex=.6 ) #cex to change fond of the figure

#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################

library( pls )

College2 = College[,-1]

x = model.matrix( Apps ~ . , data=College2 )[,-1]
y = College2$Apps

set.seed( 100 )
train = sample( 1:nrow(x) , nrow(x)/2 )
test = (-train)
x.test = x[test,]
y.test = y[test]

#############################################################################################################

##Principal Components Regression (PCR)

 #perform 10-fold cross-validation (on training data) to determine the number of components and estimate the test error (on validation data)
set.seed( 9999 )
pcr_fit_train = pcr( Apps ~ . , data = College2 , subset=train , scale=TRUE , validation="CV" )
summary( pcr_fit_train )
validationplot( pcr_fit_train , val.type="MSEP" )

pcr_predict = predict( pcr_fit_train , x.test , ncomp=16 )
pcr_mse = mean( ( pcr_predict - y.test ) ^2 )
pcr_mse

#fit PCR with 16 components to full data
pcr_fit_full = pcr( y ~ x , scale=TRUE , ncomp=16 )
summary( pcr_fit_full )
pcr_fit_full$loadings
pcr_fit_full$Yloadings

#############################################################################################################
#############################################################################################################

##Partial Least Squares (PLS) regression

#perform 10-fold cross-validation (on training data) to determine the number of components and estimate the test error (on validation data)
set.seed( 500 )
pls_fit_train = plsr( Apps ~ . , data = College2 , subset=train , scale=TRUE , validation="CV" )
summary( pls_fit_train )
validationplot( pls_fit_train , val.type="MSEP" )

pls_predict = predict( pls_fit_train , x.test , ncomp=5 )
pls_mse = mean( ( pls_predict - y.test ) ^2 )
pls_mse

#fit PLS with 2 components to full data
pls_fit_full = plsr( y ~ x , scale=TRUE , ncomp=5 )
summary( pls_fit_full )
pls_fit_full$loadings
pls_fit_full$Yloadings