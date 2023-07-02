library(ISLR)
library(leaps)
library(dplyr)
library(tidyverse)
library(caret)

# Load the data
house <- read.csv("BostonHousing.csv")

# Review summary of the data set
str(house)

# Display the head of the data set
head(house)

# Fit regression model and print summary output

# YOUR CODE HERE
regfit_full = regsubsets(medv~.,house, nvmax = 12)
reg_summary = summary(regfit_full)

# Review elements available in the summary
names(reg_summary)

# Review the R-squared and adjusted R-squared outputs
reg_summary$rsq
reg_summary$adjr2

# Plot required elements
# YOUR CODE HERE
par(mfrow=c(2,2))
plot(reg_summary$rss, xlab="Number of Variables", ylab="RSS")

## Plot adjusted R-squared vs number of variables
# YOUR CODE HERE
plot(reg_summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
(best_model = which.max(reg_summary$adjr2))
points(best_model, reg_summary$adjr2[best_model], col="red", cex=2, pch=20)
## In a similar fashion, plot Cp and BIC
# YOUR CODE HERE
plot(reg_summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
(best_model = which.min(reg_summary$cp))
points(best_model, reg_summary$cp[best_model], col="red", cex=2, pch=20)

plot(reg_summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
(best_model = which.min(reg_summary$bic))
points(best_model, reg_summary$bic[best_model], col="red", cex=2, pch=20)


# Plot using regsubsets's built-in plot function
par(mfrow=c(2,2))
# YOUR CODE HERE
plot(regfit_full, scale="r2")
plot(regfit_full, scale="adjr2")
plot(regfit_full, scale="Cp")
plot(regfit_full, scale="bic")

# Print the coefficients
coef(regfit_full, id = 10)

# Use regsubsets to perform FSS
# YOUR CODE HERE
regfit_fwd = regsubsets(medv ~ .,
                        data = house,
                        nvmax = 10,
                        method = "forward")

# Print the summary
# YOUR CODE HERE
summary(regfit_fwd)

# Plot the output
# YOUR CODE HERE
par(mfrow = c(2, 2))
plot(regfit_fwd, scale = "r2")
plot(regfit_fwd, scale = "adjr2")
plot(regfit_fwd, scale = "Cp")
plot(regfit_fwd, scale = "bic")

# Use regsubsets to perform BSS
regfit_bwd = regsubsets(medv ~ .,
                        data = house,
                        nvmax = 10,
                        method = "backward")

# Print the summary
summary(regfit_bwd)

# Plot the output
par(mfrow = c(2, 2))
plot(regfit_bwd, scale = "r2")
plot(regfit_bwd, scale = "adjr2")
plot(regfit_bwd, scale = "Cp")
plot(regfit_bwd, scale = "bic")

# Review coefficient outputs for the models
# YOUR CODE HERE
# Review coefficient outputs for our models
coef(regfit_full, 10)
coef(regfit_fwd, 10)
coef(regfit_bwd, 10)
