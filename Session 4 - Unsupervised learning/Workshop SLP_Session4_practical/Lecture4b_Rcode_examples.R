rm( list=ls() )
cat('\014')

##K-means cluster analysis --------

  #generate data
set.seed( 148 )
x = matrix( rnorm(200) , 100 , 2 ) # kind of noise (causes within-cluster variation)
xmean = matrix( rnorm( 8 , sd=4 ) , 4 , 2 ) # generate cluster means (centroids)
xmean
trueclusters = sample( 1:4 , 100 , replace=TRUE ) # generate a clustering
trueclusters
xmean[ trueclusters , ]
x = x + xmean[ trueclusters , ] #shift the guassian distributions

plot( x , col=trueclusters , pch=19 )

  #perform k-means analysis
kmeans_result = kmeans( x , 4 , nstart=100 ) # 100 random starts
kmeans_result

kmeans_result$cluster
kmeans_result$size
kmeans_result$centers
xmean #compare with true cluster centroids

kmeans_result$withinss # within-cluster variation (per cluster)
kmeans_result$tot.withinss # total within-cluster variation
kmeans_result$totss # total variation in the data
kmeans_result$iter # number of iterations before convergence

 # compute model fit
kmeans_result$Prop_explained = ( kmeans_result$totss - kmeans_result$tot.withinss ) / kmeans_result$totss #a kind of model fit (like an R-squared value)
kmeans_result$Prop_explained

 # the cluster labels are arbitrary (i.e., which cluster is cluster 1, 2, ...)
plot( x , col=kmeans_result$cluster , cex=2 , pch=1 , lwd=2 ) #plot the solution
points( x , col=trueclusters , pch=19 ) #also plot the true clustering (and compare)
table( kmeans_result$cluster , trueclusters ) # there are only 2 mistakes

 ## let's try for different values of K
nClustersMax = 10
Prop_expl_vector = matrix( -9999 , 1 , nClustersMax )
SSerrors_vector = matrix( -9999 , 1 , nClustersMax )
kmeans_res = list()
for( tel in 1:nClustersMax )
{
  kmeans_res[[tel]] = kmeans( x , tel , nstart=500 )
  prop_expl = ( kmeans_res[[tel]]$totss - kmeans_res[[tel]]$tot.withinss ) / kmeans_res[[tel]]$totss
  Prop_expl_vector[ tel ] = prop_expl
  SSerr = sum( ( x - kmeans_res[[tel]]$centers[ kmeans_res[[tel]]$cluster , ] ) ^ 2 )
  SSerrors_vector[ tel ] = SSerr
  rm( prop_expl , SSerr )
}
Prop_expl_vector
SSerrors_vector

 # scree-type of plots
par(mfrow=c(1,2))
plot( 1:nClustersMax , Prop_expl_vector , type="b" , xlab="number of clusters" , ylab="Proportion explained" )
plot( 1:nClustersMax , SSerrors_vector , type="b" , xlab="number of clusters" , ylab="SSerror" )

#############################################################################################################
#############################################################################################################

##Hierarchical clustering (bottom-up) -------

 # complete linkage
hierclust_result_complete = hclust( dist(x) , method="complete" ) #based on largest distance between points from different clusters
str( hierclust_result_complete )
hierclust_result_complete$merge

 # plot dendrogram
par(mfrow=c(1,1))
plot( hierclust_result_complete )

 # single linkage
hierclust_result_single = hclust( dist(x) , method="single" ) #based on smallest distance
plot( hierclust_result_single )

 # average linkaga
hierclust_result_average = hclust( dist(x) , method="average" ) #based on average distance between all pairs of points
plot( hierclust_result_average )

?hclust # are there other types of linkage

 # cut the dendrogram into 4 clusters (and compare with the true clusters and the k-means clusters)
hierclust_cut4_complete = cutree( hierclust_result_complete , 4 )
hierclust_cut4_complete
table( hierclust_cut4_complete , trueclusters )
plot( hierclust_result_complete , labels=trueclusters )
table( hierclust_cut4_complete , kmeans_result$cluster )

hierclust_cut4_single = cutree( hierclust_result_single , 4 )
table( hierclust_cut4_single , trueclusters )
plot( hierclust_result_single , labels=trueclusters )
table( hierclust_cut4_single , kmeans_result$cluster )

hierclust_cut4_average = cutree( hierclust_result_average , 4 )
table( hierclust_cut4_average , trueclusters )
plot( hierclust_result_average , labels=trueclusters )
table( hierclust_cut4_average , kmeans_result$cluster )

 # let's use correlations as 'distances' (instead of Euclidean distances)
x2 = matrix( rnorm(40) , 5 , 8 )
dist(x2) #distances between cases
dim( dist(x2) ) # it's not a matrix
length( dist(x2) ) # it seems all distances are collected in a vector

dim( cor(x2) ) # these are the correlations among the variables (across the samples)
dim( cor( t(x2) ) ) # I want the correlations among the samples (across the variables)
corr_samples = cor( t(x2) )
corr_samples

hierclust2_result_complete = hclust( corr_samples , method="complete" ) #corr_samples does not have the correct form
hierclust2_result_complete = hclust( as.dist(1 - corr_samples) , method="complete" ) # a large correlation means a low dissimilarities
plot( hierclust2_result_complete )
abline( h = .6 , col="red" )
hierclust2_result_complete$merge

labs = c( "nl" , "de" , 3 , 2 , 1 ) # I used some random labels for the 5 cases
plot( hierclust2_result_complete , labels=labs )
abline( h = .6 , col="red" )