library(tidyverse)
library(margins)
library(ISLR)

data <- Smarket

str(data)
head(data)

# Plot the traded volumes over time
plot(data$Volume)

library(caret)
x <- data[,2:7]
y <- data[,9]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# YOUR CODE HERE
logitReg <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = data, 
    family = binomial(link = logit))


# YOUR CODE HERE
summary(logitReg)


# YOUR CODE HERE
summary(margins(logitReg))

# Predict the next five values
glm.probs <- predict(logitReg,type = "response")
glm.probs[1:5]

# Assign values of "up" and "down" based on the probabilities predicted
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

# Print confusion matrix
# YOUR CODE HERE
table(glm.pred)

# Print the error rate
mean(glm.pred != data$Direction)

# YOUR CODE HERE
predict(logitReg, newdata = data.frame(Lag1 = c(0.5), Lag2 = c(-0.5),Lag3 = c(-5), Lag4=c(-1), Lag5 = c(1),Volume=c(5), 
    type = "response")
