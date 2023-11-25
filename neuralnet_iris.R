library(dplyr)
library(caret)
library(neuralnet)

# Загрузка данных
iris<-read.table("iris.csv", header=T, sep=",")
summary(iris)

# Преобразование в факторы
iris$variety <- as.factor(iris$variety)

# Нормализация данных
num_cols <- sapply(iris, is.numeric)
iris[num_cols] <- scale(iris[num_cols])

# Разделение данных на обучающую и тестовую выборки
set.seed(42)
trainIndex <- createDataPartition(iris$variety, p = .4, list = FALSE, times = 1)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Создание списка предикторов и сохранение столбца отдельно
predictors <- setdiff(names(trainData), "variety")
trainDatavariety <- trainData$variety
testDatavariety <- testData$variety

# Кодирование факторов для использования в neuralnet
trainDataTransformed <- dummyVars(~ ., data = select(trainData, predictors))
trainDataNN <- data.frame(predict(trainDataTransformed, newdata = trainData))
trainDataNN$variety <- trainDatavariety

testDataTransformed <- dummyVars(~ ., data = select(testData, predictors))
testDataNN <- data.frame(predict(testDataTransformed, newdata = testData))
testDataNN$variety <- testDatavariety

# Создание формулы для модели
formula_variety <- reformulate(termlabels = names(trainDataNN)[-which(names(trainDataNN) == "variety")], response = "variety")

# Обучение модели neuralnet
model_variety <- neuralnet(formula_variety, data = trainDataNN, hidden = c(5), linear.output = FALSE)

# Предсказание региона на тестовых данных
testDataNNPredict <- testDataNN
testDataNNPredict$variety <- NULL  # Удаление variety перед предсказанием
predictions_variety <- compute(model_variety, testDataNNPredict)
predicted_labels_variety <- apply(predictions_variety$net.result, 1, which.max)
predicted_variety <- levels(testData$variety)[predicted_labels_variety]

# Вычисление точности (accuracy) для variety
correct_predictions <- sum(predicted_variety == testDatavariety)
accuracy <- correct_predictions / length(testDatavariety)
cat("Accuracy for variety:", accuracy, "\n")

plot(model_variety)

testData$predicted_variety <- predicted_variety
head(testData)

