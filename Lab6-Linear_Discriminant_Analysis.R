# Load necessary libraries
library(ggplot2)
library(MASS)
library(mda)
library(caret)

# Load and inspect the iris dataset
data(iris)
df <- iris
head(df)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train LDA model
lda_model <- lda(Species ~ ., data = train_data)
lda_predictions <- predict(lda_model, newdata = test_data)$class

# Train QDA model
qda_model <- qda(Species ~ ., data = train_data)
qda_predictions <- predict(qda_model, newdata = test_data)$class

# Train MDA model
mda_model <- mda(Species ~ ., data = train_data)
mda_predictions <- predict(mda_model, newdata = test_data)

# Evaluate the models
lda_confusion <- confusionMatrix(lda_predictions, test_data$Species)
qda_confusion <- confusionMatrix(qda_predictions, test_data$Species)
mda_confusion <- confusionMatrix(mda_predictions, test_data$Species)

# Print the accuracies
cat("LDA Accuracy:", lda_confusion$overall['Accuracy'], "\n")
cat("QDA Accuracy:", qda_confusion$overall['Accuracy'], "\n")
cat("MDA Accuracy:", mda_confusion$overall['Accuracy'], "\n")

# Plot the results
plot_lda <- ggplot(test_data, aes(Sepal.Length, Sepal.Width, color = lda_predictions)) +
  geom_point() +
  ggtitle("LDA Predictions")

plot_qda <- ggplot(test_data, aes(Sepal.Length, Sepal.Width, color = qda_predictions)) +
  geom_point() +
  ggtitle("QDA Predictions")

plot_mda <- ggplot(test_data, aes(Sepal.Length, Sepal.Width, color = mda_predictions)) +
  geom_point() +
  ggtitle("MDA Predictions")

grid.arrange(plot_lda, plot_qda, plot_mda, nrow = 2)
