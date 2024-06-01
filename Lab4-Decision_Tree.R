# Load necessary libraries
library(ISLR)
library(rpart)
library(rpart.plot)
library(caret)

# Load and inspect the data
data(Carseats)
hist(Carseats$Sales)

# Create the High variable
High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(Carseats), size = 0.7 * nrow(Carseats))
train_data <- Carseats[train_indices, ]
test_data <- Carseats[-train_indices, ]

# Train a full decision tree (no pre-pruning)
full_tree <- rpart(High ~ . - Sales, data = train_data, method = 'class', 
                   control = rpart.control(cp = 0, maxdepth = 30, minsplit = 2))

# Plot the full tree
rpart.plot(full_tree)

full_predictions <- predict(full_tree, test_data, type = "class")
full_predictions <- factor(full_predictions, levels = c("No", "Yes"))
test_data$High <- factor(test_data$High, levels = c("No", "Yes"))

full_confusion <- confusionMatrix(full_predictions, test_data$High)
full_accuracy <- full_confusion$overall['Accuracy']
cat("Accuracy of the full tree:", full_accuracy, "\n")

cp_values <- full_tree$cptable[,"CP"]
accuracies <- sapply(cp_values, function(cp) {
  pruned_tree <- prune(full_tree, cp = cp)
  pruned_predictions <- predict(pruned_tree, test_data, type = "class")
  pruned_predictions <- factor(pruned_predictions, levels = c("No", "Yes"))
  pruned_confusion <- confusionMatrix(pruned_predictions, test_data$High)
  return(pruned_confusion$overall['Accuracy'])
})

best_cp <- cp_values[which.max(accuracies)]
best_pruned_tree <- prune(full_tree, cp = best_cp)
best_pruned_predictions <- predict(best_pruned_tree, test_data, type = "class")
best_pruned_predictions <- factor(best_pruned_predictions, levels = c("No", "Yes"))
best_pruned_confusion <- confusionMatrix(best_pruned_predictions, test_data$High)
best_pruned_accuracy <- best_pruned_confusion$overall['Accuracy']

cat("Best accuracy after post-pruning:", best_pruned_accuracy, "\n")

rpart.plot(best_pruned_tree)
