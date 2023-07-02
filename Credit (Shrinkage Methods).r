# Set the seed and load packages required
set.seed(123)
library(tidyverse)
library(glmnet)
library(caret)     
library(ISLR)
library(psych)

# Load and prepare the data
Credit <- Credit
Credit <- select(Credit,-c(ID)) # Remove the ID
Credit <- na.omit(Credit)         # Make sure that there are no NA values

y <- Credit %>% select(Balance) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
x <- model.matrix( ~ . - 1, select(Credit,-c(Balance)))

# Execute 10-fold cross-validation to select the optimal lambda
lambdas_to_try <- 10^seq(0, 10, length.out = 100)

# Set alpha to 0 to implement ridge regression
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)

# Plot the results
plot(ridge_cv)

# Determine the best cross-validated lambda for the ridge regression example
lambda_cv <- ridge_cv$lambda.min

# Fit the final model, and calculate RSS and multiple R-squared values
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, x)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_ridge_cv <- cor(y, y_hat_cv)^2

# Determine how the coefficients are shrunk by the increasing lambda
res <- glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Execute 10-fold cross-validation to select the optimal lambda
lambdas_to_try <- 10^seq(0, 10, length.out = 100)

# Perform the lasso method by setting alpha = 1
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot the results
plot(lasso_cv)

# Determine the best cross-validated lambda for the lasso example
lambda_cv <- lasso_cv$lambda.min

# Fit the final model, and calculate RSS and multiple R-squared values
model_cv2 <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv2 <- predict(model_cv2, x)
ssr_cv2 <- t(y - y_hat_cv2) %*% (y - y_hat_cv2)
rsq_lasso_cv2 <- cor(y, y_hat_cv2)^2

# Determine how the coefficients are shrunk by the increasing lambda
res2 <- glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res2, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Calculate R-squared values for the different models for comparison
rsq <- cbind("R-squared" = c(rsq_ridge_cv, rsq_lasso_cv2))
rownames(rsq) <- c("ridge cross-validated", "lasso cross_validated")
print(rsq)

# Repeat the plots of solution paths for comparison
par(mfrow=c(2,2))

# Ridge - top left
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Ridge - top right
plot(ridge_cv, main="Ridge",cex=0.7)

# Lasso - bottom left
plot(res2, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Lasso - bottom right
plot(lasso_cv, main="LASSO",cex=0.7)

# Obtain the lambda 1 standard deviation away from the local extrema
lambda_cv2 <- ridge_cv$lambda.1se # Can use "min" instead of "1se" to obtain the minimum value

# Display the model coefficients
coef(ridge_cv, s="lambda.1se")


# Predict using the cross-validated model
predict(ridge_cv, newx = x[1:10,], s = "lambda.1se")

# Predict using the model fitted on all the data
predict(model_cv, newx = x[1:10,], s = "lambda.1se")

# Obtain the lambda 1 standard deviation away from the local extrema
lambda_cv2 <- lasso_cv$lambda.1se # Can use "min" instead of "1se" to obtain the minimum value

# Display the model coefficients
coef(lasso_cv, s="lambda.1se")

# Predict using the cross-validated model
predict(lasso_cv, newx = x[1:10,], s = "lambda.1se")

# Predict using the model fitted on all the data
predict(model_cv2, newx = x[1:10,], s = "lambda.1se")
