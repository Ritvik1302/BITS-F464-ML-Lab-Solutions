library(e1071) 
library(ggplot2) 
library(caret)
library(ISLR)
library(ROCR)

data(Auto)
Auto <- na.omit(Auto)
head(Auto)
Auto$mpg <- ifelse(Auto$mpg >= median(Auto$mpg), 1, 0)
Auto$mpg<- as.numeric(Auto$mpg)

set.seed(123)
train.index <- createDataPartition(Auto$mpg, p = 0.7, list = FALSE)
train <- Auto[train.index, ]
test <- Auto[-train.index, ]

svm.fit <- svm(mpg ~ ., data = train, kernel = "linear",cost=10,scale=TRUE)
summary(svm.fit)
plot(svm.fit, train)

tune.out <- tune(svm, as.factor(mpg) ~., data=train, kernel='linear',ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
yhat <- predict(tune.out$best.model, test)

confusionMatrix(yhat, as.factor(test$mpg))

preds1 <- predict(svm.fit, newdata = test)

svm.fit2 <- svm(mpg ~ ., data = train, kernel='radial', gamma=1, cost=1e5)
summary(svm.fit2)
plot(svm.fit2, train)
set.seed(1)
tune.out <- tune(svm, as.factor(mpg) ~., data=train,kernel='radial',ranges = list(cost=c(0.1,1,10,100,1000),gamma=c(0.5, 1,2,3,4)))
summary(tune.out)

yhat <- predict(tune.out$best.model, test)
preds2 <- predict(svm.fit2, test)

confusionMatrix(yhat, as.factor(test$mpg))

