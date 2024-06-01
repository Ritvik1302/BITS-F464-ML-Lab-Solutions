library(tidyverse)
library(cluster)  
library(factoextra) 
library(gridExtra) 
library(mclust)

data(iris)
df <- iris[, -5] 
df <- scale(df)

set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

fviz_nbclust(df, kmeans, method = "silhouette")

set.seed(123)
kmeans_result <- kmeans(df, centers = 3, nstart = 25)
fviz_cluster(kmeans_result, geom = "point", data = df) + ggtitle("K-means Clustering")

pam_result <- pam(df, k = 3)
fviz_cluster(pam_result, geom = "point", data = df) + ggtitle("K-medoids Clustering")

em_result <- Mclust(df)
summary(em_result)
fviz_mclust(em_result, "classification", geom = "point", main = "EM Clustering")

plot(em_result, what = "BIC")

results <- data.frame(
  Species = iris$Species,
  KMeans = as.factor(kmeans_result$cluster),
  KMedoids = as.factor(pam_result$clustering),
  EM = as.factor(em_result$classification)
)

p1 <- fviz_cluster(kmeans_result, geom = "point", data = df) + ggtitle("K-means Clustering")
p2 <- fviz_cluster(pam_result, geom = "point", data = df) + ggtitle("K-medoids Clustering")
p3 <- fviz_mclust(em_result, "classification", geom = "point", main = "EM Clustering")

grid.arrange(p1, p2, p3, nrow = 1)

cat("K-means vs Actual Species\n")
print(table(results$KMeans, results$Species))

cat("K-medoids vs Actual Species\n")
print(table(results$KMedoids, results$Species))

cat("EM vs Actual Species\n")
print(table(results$EM, results$Species))
