#install.packages("ggplot2")
#install.packages("dbscan")
#install.packages("cluster")
library(ggplot2)
library(dbscan)
library(cluster)

# we will use the iris dataset
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  labs(title = "Initial data") +
  geom_point()

# the use of the library k-means function to petals
set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

# plot the result
table(irisCluster$cluster, iris$Species)
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + 
  labs(title = "K-means", color = "Clusters") +
  geom_point()

# the use of the library DBScan function to petals
x <- as.matrix(iris[, 3:4])
db <- dbscan(x, eps = .6, minPts = 4)
db

# plot the result
db$cluster <- as.factor(db$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = db$cluster)) + 
  labs(title = "DBScan", color = "Clusters") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=8)) +
  geom_point()

# estimation of the clusterization quality by the silhouette scope

# distance matrix with the euclidean distance measure
dist_matrix <- dist(iris[, 3:4], diag = T, method = "euclidean")
knn_sil <- mean(silhouette(as.numeric(levels(irisCluster$cluster)[irisCluster$cluster]),
                      dist_matrix)[, 3])
dbscan_sil <- mean(silhouette(as.numeric(levels(db$cluster)[db$cluster]),
                              dist_matrix)[, 3])
# more the metric, the denser the clusters
knn_sil
dbscan_sil