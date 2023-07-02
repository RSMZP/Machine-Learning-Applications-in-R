# Load the required packages and set the parameters
library(tidyverse)
library(caret)
set.seed(1)

# Load the data
data <- read_csv("Term Life Insurance.csv")

# YOUR CODE HERE
trainIndex <- createDataPartition(data$FACE, p=.7, list=F)
dataTrain <- data[trainIndex, ]
dataTest <- data[-trainIndex, ]

# YOUR CODE HERE
trainReg <- lm(FACE ~ ., data = dataTrain)
testRegPred <- predict(trainReg, newdata = dataTest)

regRMSE <- sqrt(mean((testRegPred - dataTest$FACE)^2))
regRMSE

tuneCtrl <- trainControl(method = "cv", n = 10)

nnetGrid <- expand.grid(size = c(6, 8, 10, 12, 24),
                        decay = c(0.01, 0.001, 0.1))

# YOUR CODE HERE

nnetFit <- train(FACE ~ ., 
                data = dataTrain,
                method = "nnet",
                metric = "RMSE",
                tuneGrid = nnetGrid,
                trControl = tuneCtrl,
                maxit = 200,
                linout = TRUE, 
                trace = FALSE)

# YOUR CODE HERE
nnetPredict <- predict(nnetFit, newdata = dataTest)
nnetRMSE <- sqrt(mean((nnetPredict - dataTest$FACE)^2))

nnetRMSE <- sqrt(mean((nnetPredict - dataTest$FACE)^2))
nnetRMSE

regRMSE - nnetRMSE

(regRMSE - nnetRMSE) / regRMSE

# Add test cases to a new data frame
predData <- data.frame(
  "GENDER" = 1,
  "AGE" = 44,
  "MARSTAT" = 1,
  "EDUCATION" = c(16,18,15),
  "ETHNICITY" = 2,
  "SMARSTAT" = 1,
  "SGENDER" = 1,
  "SAGE" = 33,
  "SEDUCATION" = 10,
  "NUMHH" = c(5,6,3),
  "INCOME" = c(100000,110000,90000),
  "TOTINCOME" = 200000,
  "CHARITY" = 500,
  "FACECVLIFEPOLICIES" = 0,
  "CASHCVLIFEPOLICIES" = 0,
  "BORROWCVLIFEPOL" = 0,
  "NETVALUE" = 0)

# Predict the balances for the test cases using the ANN
nnetPredict <- predict(nnetFit, newdata = predData)
nnetPredict
predData

# Predict the balances for the test cases using the regression model
regPredict <- predict(trainReg, newdata = predData)
regPredict
