# Set the seed and load the packages
set.seed(123)
library(tidyverse)
library(glmnet)
library(caret)     
library(ISLR)
library(psych)

# Load and prepare the data
house <- read.csv("BostonHousing.csv")
house=na.omit(house)         # Make sure that there are no NA values

# Split the data into separate predicted and predictor values
# Transform the categoricals using the model.matrix method and scale the predicted data
y <- house %>% select(medv) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
x <- model.matrix( ~ . - 1, select(house,-c(medv)))

# Perform 10-fold cross-validation to select lambda
lambdas_to_try <- 10^seq(0, 10, length.out = 100)

# YOUR CODE HERE
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)

# YOUR CODE HERE
plot(ridge_cv)

# YOUR CODE HERE
lambda_cv <- ridge_cv$lambda.min

# Fit final model, determine RSS, and determine multiple R-squared 
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, x)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_ridge_cv <- cor(y, y_hat_cv)^2

# See how increasing lambda shrinks the coefficients
# Lines represent coefficients per variable for different lambdas
# The higher the lambda, the more the coefficients shrink towards zero
res <- glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Perform 10-fold cross-validation to select lambda
lambdas_to_try <- 10^seq(-2, 2, length.out = 100)

# The lasso is similar to ridge regression, but set alpha = 1 instead of 0
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(lasso_cv)

# Best cross-validated lambda
lambda_cv <- lasso_cv$lambda.min

# Fit final model, determine RSS, and determine multiple R-squared
# YOUR CODE HERE
model_cv2 <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv2 <- predict(model_cv2, x)
ssr_cv2 <- t(y - y_hat_cv2) %*% (y - y_hat_cv2)
rsq_lasso_cv2 <- cor(y, y_hat_cv2)^2

# See how increasing lambda shrinks the coefficients
# The higher the lambda, the more the coefficients shrink towards zero
res2 <- glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res2, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Calculate R-squared for the different models
# YOUR CODE HERE
# Calculate R-squared values for the different models for comparison
rsq <- cbind("R-squared" = c(rsq_ridge_cv, rsq_lasso_cv2))
rownames(rsq) <- c("ridge cross-validated", "lasso cross_validated")
print(rsq)

# Repeat the plots of solution paths for comparison
par(mfrow=c(2,2))
# Ridge - left
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Ridge - right
plot(ridge_cv, main="Ridge",cex=0.7)

# Lasso - left
plot(res2, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Lasso - right
plot(lasso_cv, main="LASSO",cex=0.7)

# Obtain the lambda 1 standard deviation away from the local extrema
# YOUR CODE HERE
lambda_cv2 <- ridge_cv$lambda.1se
coef(ridge_cv, s="lambda.1se")
predict(ridge_cv, newx = x[1:10,], s = "lambda.1se")

# YOUR CODE HERE
lambda_cv2 <- lasso_cv$lambda.1se
coef(lasso_cv, s="lambda.1se")
predict(lasso_cv, newx = x[1:10,], s = "lambda.1se")
