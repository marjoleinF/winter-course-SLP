rm( list=ls() )
cat('\014')

library( ISLR )

# data set
mat = matrix( c(3,6,7,2,4,6,2,5) , 4 , 2 )
mat

#####################################################################################################################

## K-means clustering: hand calculations

 # ALS-step 1 (compute centroids) with matrix algebra
clustering = c(1,1,2,2)
I = diag(2)
I
Z = I[ clustering , ]
Z
centroids = solve( t(Z) %*% Z ) %*% t(Z) %*% mat #computational shortcut for computing cluster centroids
centroids

 # iteration 1 (step 1)
clustering = c(1,1,2,2)
Z = I[ clustering , ]
centroids = solve( t(Z) %*% Z ) %*% t(Z) %*% mat
centroids

 # iteration 1 (step 2)
distances = matrix( -9999 , 4 , 2 )
for( case in 1:dim(mat)[1] ) #for each case
{
  for( clust in 1:length(unique(clustering)) ) # for each cluster
  {
    distances[case,clust] = sum( ( mat[ case , ] - centroids[ clust , ] ) ^2 )
  }
  clustering[ case ] = which.min( distances[case,] )
}
distances
clustering

 # iteration 2 (step 1)
Z = I[ clustering , ]
centroids = solve( t(Z) %*% Z ) %*% t(Z) %*% mat
centroids

 # iteration 2 (step 2)
distances = matrix( -9999 , 4 , 2 )
for( case in 1:dim(mat)[1] ) #for each case
{
  for( clust in 1:length(unique(clustering)) ) # for each cluster
  {
    distances[case,clust] = sum( ( mat[ case , ] - centroids[ clust , ] ) ^2 )
  }
  clustering[ case ] = which.min( distances[case,] )
}
distances
clustering

######################################################################################################################
######################################################################################################################

## hierarchical clustering: hand calculations

mat = matrix( c(3,6,7,5,4,6,2,2) , 4 , 2 )
mat
dist( mat )

hc_complete = hclust( dist(mat) , method="complete" )
hc_single = hclust( dist(mat) , method="single" )
hc_average = hclust( dist(mat) , method="average" )

plot( hc_complete )
hc_complete$merge
hc_complete$height

plot( hc_single )
hc_single$merge
hc_single$height

plot( hc_average )
hc_average$merge
hc_average$height

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################

# clustering on College data

data_College = College[ , c(5,12,13) ]
data_College_stan = scale( data_College , center=TRUE , scale=TRUE )

 ## K-means clustering

set.seed( 1174 )

nClustersMax = 10
Prop_expl_vector = matrix( -9999 , 1 , nClustersMax )
SSerrors_vector = matrix( -9999 , 1 , nClustersMax )
kmeans_res = list()
for( tel in 1:nClustersMax )
{
  kmeans_res[[tel]] = kmeans( data_College_stan , tel , nstart=500 , iter.max=10 ) #maybe more than 10 iterations needed (e.g., iter.max=50)
  prop_expl = ( kmeans_res[[tel]]$totss - kmeans_res[[tel]]$tot.withinss ) / kmeans_res[[tel]]$totss
  Prop_expl_vector[ tel ] = prop_expl
  SSerr = sum( ( data_College_stan - kmeans_res[[tel]]$centers[ kmeans_res[[tel]]$cluster , ] ) ^ 2 )
  SSerrors_vector[ tel ] = SSerr
  rm( prop_expl , SSerr )
}

 # scree-type of plots
par(mfrow=c(1,2))
plot( 1:nClustersMax , Prop_expl_vector , type="b" , xlab="number of clusters" , ylab="Proportion explained" )
plot( 1:nClustersMax , SSerrors_vector , type="b" , xlab="number of clusters" , ylab="SSerror" )

 # 4-cluster solution
kmeans_res[[4]]$size
kmeans_res[[4]]$centers

# 3-cluster solution
kmeans_res[[3]]$size
kmeans_res[[3]]$centers

par(mfrow=c(2,3))
plot( data_College_stan[ , c(1,2) ] , col=kmeans_res[[4]]$cluster , pch=18 , cex=1 )
plot( data_College_stan[ , c(1,3) ] , col=kmeans_res[[4]]$cluster , pch=18 , cex=1 )
plot( data_College_stan[ , c(2,3) ] , col=kmeans_res[[4]]$cluster , pch=18 , cex=1 )
plot( data_College_stan[ , c(1,2) ] , col=kmeans_res[[3]]$cluster , pch=18 , cex=1 )
plot( data_College_stan[ , c(1,3) ] , col=kmeans_res[[3]]$cluster , pch=18 , cex=1 )
plot( data_College_stan[ , c(2,3) ] , col=kmeans_res[[3]]$cluster , pch=18 , cex=1 )

 # compare 3- and 4-cluster solution
table( kmeans_res[[3]]$cluster , kmeans_res[[4]]$cluster )

##################################################################################################################################

set.seed(422668)
data_College20 = College[ sample(1:700,20) , c(5,12,13) ]
data_College20_stan = scale( data_College20 , center=TRUE , scale=TRUE )

## hierarchical clustering

hclust_complete = hclust( dist(data_College20_stan) , method="complete" )
par(mfrow=c(1,1))
plot( hclust_complete , 1:20 )
abline( h=2.5 , col="red" )
hclust_complete_cut = cutree( hclust_complete , h=2.5 )
hclust_complete_cut

hclust_single = hclust( dist(data_College20_stan) , method="single" ) 
plot( hclust_single , 1:20 )
abline( h=1.18 , col="red" )
hclust_single_cut = cutree( hclust_single , h=1.18 )
hclust_single_cut

hclust_average = hclust( dist(data_College20_stan) , method="average" ) 
plot( hclust_average , 1:20 )
abline( h=2 , col="red" )
hclust_average_cut = cutree( hclust_average , h=2 )
hclust_average_cut

table( hclust_complete_cut , hclust_single_cut )
table( hclust_complete_cut , hclust_average_cut )
table( hclust_single_cut , hclust_average_cut )

kmeans_res2 = kmeans( data_College20_stan , 3 , nstart=500 )
table( hclust_complete_cut , kmeans_res2$cluster )
table( hclust_single_cut , kmeans_res2$cluster )
table( hclust_average_cut , kmeans_res2$cluster )