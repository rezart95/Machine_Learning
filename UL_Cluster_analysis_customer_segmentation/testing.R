# Testing enviroment

# advanced nice plotting with ggplot for points with ellipse (and outliers)
fviz_cluster(list(data=xxx, cluster=km1$cluster), ellipse.type="norm", geom="point", stand=FALSE, palette="jco", ggtheme=theme_classic()) #factoextra::


# Using factoextra:: package:
# One can use the eclust () function from the factoextra :: package. 
# It allows for clusters using k-means, PAM, CLARA etc. methods, 
# using Euclidean, Manhattan, Canberra, Minkowski distance etc.

# the same variables, different number of clusters and distance metrics
km1<-eclust(xxx, "kmeans", hc_metric="euclidean",k=3)
fviz_cluster(km1, main="kmeans / Euclidean")

km2<-eclust(xxx, "kmeans", hc_metric="manhattan", k=4)
fviz_cluster(km2, main="kmeans / Manhattan")


# clustering with triangle graphics
c2<-eclust(xxx, "pam", k= 3) # 
fviz_silhouette(c2)
fviz_cluster(c2) # 

# more clusters, other distance metric
pam2<-eclust(xxx, "pam", k=4, hc_metric="manhattan") # factoextra::
fviz_silhouette(pam2)
fviz_cluster(pam2) # 


# CLARA algorithms(CLustering LARge Applications)
# like PAM, but relies on the sampling approach and is designed for large data sets 
# PAM looks medoids for the entire data set and CLARA does the same for a sample

cl2<-eclust(xxx, "clara", k=3) # factoextra
summary(cl2)
fviz_cluster(cl2)
fviz_silhouette(cl2)

# silhouette  
# method of interpretation and validation of consistency within clusters of data
# silhouette statistics s=(bi–ai)/max(ai,bi)
# ai  average distance to all other objects in the cluster
# bi  minimum of average distance to other clusters (cluster by cluster)
# statistics is between -1 and 1
# negative s – undesirable, a>b other clusters are closer than “our” cluster
# positive s – desirable, good when a~0 (distance in our cluster), then s~1 
# more on: https://cs.fit.edu/~pkc/classes/ml-internet/silhouette.pdf 

fviz_nbclust(xxx,FUNcluster=pam) # factoextra::
fviz_nbclust(xxx, clara, method = "silhouette")+ theme_classic()


xxx.s<-center_scale(xxx) # from ClusterR:: to scale or center the data 
opt<-Optimal_Clusters_KMeans(xxx.s, max_clusters=10, plot_clusters = TRUE)
opt<-Optimal_Clusters_KMeans(xxx.s, max_clusters=10, plot_clusters=TRUE, criterion="silhouette")
opt<-Optimal_Clusters_KMeans(xxx.s, max_clusters=10, plot_clusters=TRUE, criterion="AIC")

hopkins(xxx, n=nrow(xxx)-1) # clustertend::

get_clust_tendency(xxx, 2, graph=TRUE, gradient=list(low="red", mid="white", high="blue"), seed = 123) # factoextra::

# two commands for distance (dissimilarity) are possible
# stats::dist() is basic function, output in class dist
# factoextra::get_dist() is tailored fuction, output also in class dist
# dist() allows for distances as "euclidean", "maximum",  "manhattan", "canberra", "binary" or "minkowski"
# get_dist() allows for distances as dist() and extra "pearson", "spearman" or "kendall"

d<-dist(xxx)
d<-get_dist(xxx, method = "euclidean")
fviz_dist(d, show_labels = FALSE)+ labs(title = "our data") 


#Observations / comments:
 #       -	One can see the blocks of colours at the figure, 
# what confirms that data are clusterable and clustering is feasible.

# quality of clustering for k-means
# Calinski-Harabasz index (CH)
# counter: BGSS/(K-1)  between-group sum of squares (for K clusters)
# nominator: WGSS / (N-K)  within-cluster sum of squares (sum of the within-cluster dispersions for all clusters) (for N observations)
# the higher statistics the better 
# statistic used for comparing solutions for alternative number of clusters

km1<-kmeans(xxx, 2) # stats::
round(calinhara(xxx,km1$cluster),digits=2) #fpc::calinhara()

km2<-kmeans(xxx, 3) # stats::
round(calinhara(xxx,km2$cluster),digits=2) #fpc::calinhara()

# Calinski-Harabasz is also available in fpc::cluster.stats()
dxxx<-dist(xxx)
complete3 <- cutree(hclust(dxxx),3)
c.stat<-cluster.stats(dxxx,complete3)
c.stat$ch

# Duda-Hart test for whether a data set should be split into two clusters
# for kmeans class
# H0: homogeneity of cluster (data within cluster as similar) 
# H1: heterogeneity of cluster (one can easily split the cluster)
# statistics dh: ratio of within-cluster sum of squares for two clusters and overall sum of squares.
# verification: cluster1=FALSE (H0 of homogeneity rejected, accept H1)
# verification: when dh statistics is lower than “compare” (critical value), accept H1

km1 <- kmeans(xxx,2) #fpc::
dudahart2(xxx,km1$cluster)

#Observations / comments:
 #       -	Following the Duda-Hart test we should split the clusters (as cluster1=FALSE )
# -	Following Calinski-Harabasz test, we should prefer k=3 over k=2

# statistics by clustered groups

# Clustering methods are often used in segmentation. They are to split the sample into heterogeneous groups. In consequence, as a post-action we want to see the descriptive statistics in the defined groups. By assumption, they should be different. 

# stripes for k-means
# to plot with stripes the distance of data points to cluster centroids
# stripes() works only with class kcca (from cclust())
d1<-cclust(xxx, 4, dist="euclidean") #flexclust::
stripes(d1) #flexclust::
d2<-cclust(xxx, 4, dist="manhattan") #flexclust::
stripes(d2) #flexclust::

Observations / comments:
        -	On stripes chart we see the distance of every single observation from the centroid of cluster. The higher bin the more distant locations of points within given cluster (undesirable)






