library(tidyverse)
library(cluster)
library(factoextra)
library(fpc)
europeanJobs <- read.delim("~/Downloads/europeanJobs.txt")
View(europeanJobs)
head(europeanJobs)
countries <- europeanJobs$Country
countries
df <- europeanJobs %>% select(-Country)
scaled <- scale(df)
head(scaled)
#k-mean
fviz_nbclust(scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) + 
  labs(subtitle = "Elbow Method")
fviz_nbclust(scaled, kmeans, method = "silhouette")
set.seed(123)
kmeans_result <- kmeans(scaled, centers = 3, nstart = 25)
kmeans_result
kmeans_result$cluster
sil <- silhouette(kmeans_result$cluster, dist(scaled))
plot(sil, col = 1:3, border = NA)
mean(sil[, 3])
fviz_cluster(kmeans_result, data = scaled, 
             labelsize = 8, 
             main = "K-Means Clustering") +
  theme_minimal()
plotcluster(scaled, kmeans_result$cluster)
europeanJobs$Cluster <- kmeans_result$cluster
table(europeanJobs$Cluster)
split(europeanJobs$Country, europeanJobs$Cluster)
kmeans_summary <- aggregate(. ~ kmeans_result$cluster, data = europeanJobs[, -1], FUN = mean)
kmeans_summary
#HIERARCHICAL
dist <- dist(scaled, method = "euclidean")
hclust <- hclust(dist, method = "ward.D2")
hclust
plot(hclust, labels = europeanJobs$Country, main = "Hierarchical Clustering Dendrogram", 
     xlab = "", sub = "", ylab = "Height")
h3clust = cutree(hclust,k=3)
plotcluster(hclust, h3clust)
table(KMeans = kmeans_result$cluster, Hierarchical = hc_clusters)
plotcluster(scaled, h3clust)
