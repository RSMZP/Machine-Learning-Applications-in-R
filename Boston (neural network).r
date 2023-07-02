options(warn=-1)

library(tidyverse)
library(caret)
set.seed(123)

data <- read_csv("BostonHousing.csv")

trainIndex <- createDataPartition(data$medv, p=.7, list=F)
dataTrain <- data[trainIndex, ]
dataTest <- data[-trainIndex, ]

trainReg <- lm(medv ~ ., data = dataTrain)

testRegPred <- predict(trainReg, newdata = dataTest)

regRMSE <- sqrt(mean((testRegPred - dataTest$medv)^2))
regRMSE

tuneCtrl <- trainControl(method = "cv", n = 10)

nnetGrid <- expand.grid(size = c(6, 8, 10, 12, 24),
                        decay = c(0.01, 0.001, 0.1))

nnetFit <- train(medv ~ ., 
                data = dataTrain,
                method = "nnet",
                metric = "RMSE",
                tuneGrid = nnetGrid,
                trControl = tuneCtrl,
                maxit = 200,
                linout = TRUE, 
                trace = FALSE)

nnetPredict <- predict(nnetFit, newdata = dataTest)
nnetRMSE <- sqrt(mean((nnetPredict - dataTest$medv)^2))

regRMSE
nnetRMSE
