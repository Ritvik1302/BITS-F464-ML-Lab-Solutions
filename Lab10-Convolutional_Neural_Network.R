library(keras)
mnist <- dataset_mnist()

train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

class_names <- 0:9

dim(train_images)

dim(train_labels)

train_labels[1:20]

train_images <- train_images / 255
test_images <- test_images / 255

img <- train_images[1, , ]
img <- t(apply(img, 2, rev) )
image(x = 1:28, y = 1:28, z = img, col = gray((0:255) / 255), xaxt = "n", yaxt = "n")

model <- keras_model_sequential() %>% # creating a model in sequential mode
  layer_flatten(input_shape = c(28, 28) ) %>% # 2d-array to 1d-array
  layer_dense(units = 128, activation = "relu") %>% # densely-connected layer
  layer_dropout(rate = 0.3) %>% # using dropout to reduce overfitting
  layer_dense(units = 10, activation = "softmax") # predicting the class probability

summary(model)

model %>% compile(
  loss = "sparse_categorical_crossentropy", # loss function to be minimised
  optimizer = "adam", # how the model is updated
  metrics = "accuracy" # used to monitor the training
)

history <- model %>% fit(
  x = train_images,
  y = train_labels,
  epochs = 10,
  validation_split = 0.2,
  verbose = 2
)

model %>% evaluate(test_images, test_labels)

predictions <- model %>% predict(test_images)

class_pred <- model %>% predict(test_images)%>%k_argmax()
class_pred[1:20]
img <- test_images[1, , , drop = FALSE]
dim(img)
class_pred <- model %>% predict(img) %>% k_argmax()
class_pred <- as.array(class_pred)
class_pred
class_names[class_pred + 1]
