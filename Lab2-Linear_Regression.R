library(MASS)
library(ggplot2)

data("Boston")
head(Boston)
str(Boston)

set.seed(123)

sample_size <- floor(0.7 * nrow(Boston))
train_indices <- sample(seq_len(nrow(Boston)), size = sample_size)

train_data <- Boston[train_indices, ]
test_data <- Boston[-train_indices, ]

linear_model <- lm(medv ~ ., data = train_data)

predictions <- predict(linear_model, test_data)

mse <- mean((test_data$medv - predictions)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

r_squared <- 1 - sum((test_data$medv - predictions)^2) / sum((test_data$medv - mean(test_data$medv))^2)
cat("R-squared:", r_squared, "\n")

plot_data <- data.frame(actual = test_data$medv, predicted = predictions)

ggplot(plot_data, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Values",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal()