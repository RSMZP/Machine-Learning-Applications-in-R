library(tidyverse)
library(margins)  #to obtain the average marginal effects

credit_data <- read.csv("Credit.csv")

str(credit_data)
head(credit_data)

credit_data$default <- as.factor(credit_data$default)
credit_data$student <- as.factor(credit_data$student)
str(credit_data)

logitReg <- glm(default ~., data = credit_data, family = binomial(link = logit))
summary(logitReg)

summary(margins(logitReg))

newData <- data.frame("Yes", 2000, 30000)
colnames(newData) <- c("student", "balance", "income")
predict(logitReg, newData, type = "response")
