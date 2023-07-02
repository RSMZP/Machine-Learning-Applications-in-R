library(ISLR)
library(leaps)
library(dplyr)

# Load the data
credit <- Credit

# Review summary of the data set
str(credit)

# Display the head of the data set
head(credit)

credit <- credit %>% select(-ID)

head(credit)

# Fit regression model and print summary output
regfit_full = regsubsets(Balance ~ ., data = credit, nvmax = 10)
reg_summary = summary(regfit_full)

# Review elements available in the summary
names(reg_summary)

# Review the R-squared and adjusted R-squared outputs
reg_summary$rsq
reg_summary$adjr2

# Plot required elements
par(mfrow=c(2,2))
plot(reg_summary$rss, xlab="Number of Variables", ylab="RSS")

## Plot adjusted R2 vs Number of Variables
plot(reg_summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
## which_max() function is used to identify the location of the maximum point of a vector
(best_model = which.max(reg_summary$adjr2))
## Plot a red dot to indicate the model with the largest adjusted R2
points(best_model, reg_summary$adjr2[best_model], col="red", cex=2, pch=20)

## In a similar fashion, we can plot Cp and BIC
plot(reg_summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
(best_model = which.min(reg_summary$cp))
points(best_model, reg_summary$cp[best_model], col="red", cex=2, pch=20)

plot(reg_summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
(best_model = which.min(reg_summary$bic))
points(best_model, reg_summary$bic[best_model], col="red", cex=2, pch=20)

# Plot using regsubsets built-in plot function
par(mfrow=c(2,2))
plot(regfit_full, scale="r2")
plot(regfit_full, scale="adjr2")
plot(regfit_full, scale="Cp")
plot(regfit_full, scale="bic")

# Print the coefficients
coef(regfit_full, id = 4)

# Use regsubsets() to perform forward stepwise selection
regfit_fwd = regsubsets(Balance ~ .,
                        data = credit,
                        nvmax = 10,
                        method = "forward")
# Print the summary
summary(regfit_fwd)

# Plot the output
par(mfrow = c(2, 2))
plot(regfit_fwd, scale = "r2")
plot(regfit_fwd, scale = "adjr2")
plot(regfit_fwd, scale = "Cp")
plot(regfit_fwd, scale = "bic")

# Use regsubsets() to perform backward stepwise selection
regfit_bwd = regsubsets(Balance ~ .,
                        data = credit,
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

# Review coefficient outputs for our models
coef(regfit_full, 7)
coef(regfit_fwd, 7)
coef(regfit_bwd, 7)
