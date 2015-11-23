library(rpart)
library(randomForest)
library(e1071)

data <- read.csv("train.csv", header = T, sep = ",")
indices = sample(1:nrow(data), size = 0.8*nrow(data))
training = data[indices, ]
test = data[-indices, ]

training <- as.data.frame(training)
test <- as.data.frame(test)

fmla = formula(as.factor(Survived) ~ Pclass + Sex + SibSp + Parch + Fare)
tree = rpart(fmla, data = training, method = "class")
pred_tree = predict(tree, newdata = test, type = "class")
accuracy_tree = sum(pred_tree == test$Survived) / nrow(test)


rf = randomForest(fmla, data = training)
pred_rf = predict(rf, newdata = test, type = "class")
accuracy_rf = sum(pred_rf == test$Survived) / nrow(test)
importance(rf)


fmla2 = formula(as.factor(Survived) ~  Pclass + Sex + SibSp + Parch + Fare + Embarked )
tree2 = rpart(fmla2, data = training, method = "class")
pred_tree2 = predict(tree2, newdata = test, type = "class")
accuracy_tree2 = sum(pred_tree2 == test$Survived) / nrow(test)

svm = svm(fmla, data = training)
pred_svm = predict(svm, newdata = test, type = "class")
accuracy_svm = sum(pred_svm == test$Survived) / nrow(test)
