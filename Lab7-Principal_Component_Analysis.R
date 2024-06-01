library(ggplot2)
library(ggcorrplot)
library(MultBiplotR)

iris_pca <- princomp(iris[, -5])
summary(iris_pca)
iris_pca$loadings[, 1:2]

screeplot(iris_pca)

biplot(iris_pca)

mtcars_pca <- princomp(mtcars)
summary(mtcars_pca)
mtcars_pca$loadings[, 1:2]  

screeplot(mtcars_pca)

biplot(mtcars_pca)
