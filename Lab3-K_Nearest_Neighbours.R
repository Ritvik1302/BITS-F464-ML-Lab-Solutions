# Load necessary libraries
library(class)
library(caret)

iris.new <- iris[c('Species','Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')]
str(iris.new)
head(iris.new)

# Normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to the feature columns
iris.n <- as.data.frame(lapply(iris.new[,2:5], normalize))
head(iris.n)

# Add the Species column back to the normalized data
iris.n$Species <- iris.new$Species

# Split the data into training and testing sets
set.seed(123)
dat.d <- sample(1:nrow(iris.n), size = nrow(iris.n) * 0.7, replace = FALSE)

# 70% training data
train.iris <- iris.n[dat.d, ]
# Remaining 30% test data
test.iris <- iris.n[-dat.d, ]

# Creating separate dataframe for 'Species' feature which is our target
train.iris_labels <- train.iris$Species
test.iris_labels <- test.iris$Species

# Removing the Species column from train and test datasets
train.iris <- train.iris[, -5]
test.iris <- test.iris[, -5]

# Check number of rows in training labels
NROW(train.iris_labels)

# Train KNN model and make predictions
knn.10 <- knn(train = train.iris, test = test.iris, cl = train.iris_labels, k = 10)
knn.11 <- knn(train = train.iris, test = test.iris, cl = train.iris_labels, k = 11)

# Calculate the proportion of correct classification for k = 10, 11
ACC.10 <- 100 * sum(test.iris_labels == knn.10) / NROW(test.iris_labels)
ACC.11 <- 100 * sum(test.iris_labels == knn.11) / NROW(test.iris_labels)

# Print accuracy
ACC.10
ACC.11

# Check prediction against actual value in tabular form for k = 10
table(knn.10, test.iris_labels)

# Check prediction against actual value in tabular form for k = 11
table(knn.11, test.iris_labels)

# Create confusion matrix for k = 10
confusionMatrix(table(knn.10, test.iris_labels))

# Optimization
k.optm <- integer(12)
for (i in 1:12) {
  knn.mod <- knn(train = train.iris, test = test.iris, cl = train.iris_labels, k = i)
  k.optm[i] <- 100 * sum(test.iris_labels == knn.mod) / NROW(test.iris_labels)
  cat(i, '=', k.optm[i], '\n')
}

# Plot the accuracy level vs K-value
plot(k.optm, type = "b", xlab = "K-Value", ylab = "Accuracy level")
