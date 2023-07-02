library(e1071)
library(caret)

class <- readRDS("Class.RDS")
dtm <- readRDS("dtm.RDS")

trainClass <- class[1:1500]
testClass <- class[1501:2000]

trainDTM <- dtm[1:1500, ]
testDTM <- dtm[1501:2000, ]

classifier <- naiveBayes(trainDTM, trainClass)

testPreds <- predict(classifier, newdata = testDTM)

confusionMatrix(testPreds, testClass)
