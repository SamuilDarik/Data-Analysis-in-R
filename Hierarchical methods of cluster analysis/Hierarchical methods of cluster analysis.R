#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("cluster")
#install.packages("nnet")
library(ggplot2)
library(dplyr)
library(cluster)
library(nnet)

# plot initiak data (iris dataset - petals)
ggplot(iris, aes(Petal.Length, Petal.Width)) +
  labs(title = "Initial data") +
  geom_point()

# distance matrix with the euclidean distance measure
dist_matrix <- dist(iris[, 3:4], diag = T, method = "euclidean")

# the use of the library function for hierarchical cluster analysis with the avearge agglomeration method
clusters <- hclust(dist_matrix, method = "average")

# plot cluster dendrogram
plot(clusters, cex=0.6)

# estimate the quality of clusterization by silhouette scope on each step
sil <- rep(0, nrow(iris)-2)
for (i in (nrow(iris)-2):1) {
  sub_group <- cutree(clusters, k = as.numeric(i+1))
  x <- iris[, 3:4] %>% mutate(cluster = sub_group)
  sil[nrow(iris)-1-i] <- mean(silhouette(x$cluster, dist_matrix)[, 3])
}
df.sil <- as.data.frame(sil)

# the change of the silhouette scope on each step
ggplot(df.sil, aes(x = c(1:(nrow(iris)-2)), y = df.sil$sil, group = 1)) + 
  geom_point(shape=21, fill="red", color="darkred", size=2) + 
  geom_line(color="red", size=1) +
  labs(title = "Silhouette", x = "", y = "")

# visualize the a splitting corresponding to the best metric value

max(sil)
which.is.max(sil)

sub_group <- cutree(clusters, k = (nrow(iris)-which.is.max(sil)))
x <- iris[, 3:4] %>% mutate(cluster = sub_group)
x$cluster <- as.factor(x$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = x$cluster)) + 
  labs(title = "Best splitting", color = "Cluster") +
  geom_point()