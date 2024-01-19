# examples (chapter 10)

rm( list=ls() )
cat('\014')

############################################################################################################
############################################################################################################

### Generate data for clustering
set.seed( 148 )
x = matrix( rnorm(200) , 100 , 2 ) # kind of noise (causes within-cluster variation)
xmean = matrix( rnorm( 8 , sd=4 ) , 4 , 2 ) # generate cluster means (centroids)
xmean
trueclusters = sample( 1:4 , 100 , replace=TRUE ) # generate a clustering
trueclusters
xmean[ trueclusters , ]
x = x + xmean[ trueclusters , ] #shift the guassian distributions

plot( x , col=trueclusters , pch=19 )

#############################################################################################################
#############################################################################################################

#model-based clustering (mixture approach for clustering based on a likelihood)

xdata = x

library(mclust)
library(mclustcomp)
?mclust

 #Select optimal model (i.e., shape of variance-covariance matrix)
 # and number of clusters
Res_MBC <- mclustBIC( xdata , G=1:9 )
summary(Res_MBC)
Res_MBC
plot(Res_MBC)

 #Inspect best solution
Res_MBC2 <- Mclust( xdata , x = Res_MBC )
summary( Res_MBC2 , parameters = TRUE )
plot( Res_MBC2 , what = "classification" )