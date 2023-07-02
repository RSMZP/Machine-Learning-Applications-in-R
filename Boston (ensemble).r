# Load required packages.
library(tree)
library(MASS)
# randomForest and gbm packages are installed later on.

# Load the Boston data set from the MASS package.
library(MASS)
Boston <- Boston
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0)

# Perform cross-validation.
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")

# Demonstrate pruning using a manual parameter.
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

# Predict using the unpruned tree on the test data.
yhat <- predict(tree.boston, newdata = Boston[-train,])

# True values on the testing data.
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)

# Compute the test MSE
mean((yhat - boston.test) ^ 2)

# Load the randomForest package, and fit the model.
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv~., 
                           data=Boston, 
                           subset=train, 
                           mtry=13, 
                           importance=TRUE)
bag.boston

# Demonstrate bagging.
yhat.bag <-predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)

# Compute the test mean square of errrors.
mean((yhat.bag-boston.test)^2)

# Review the variable importance.
importance(bag.boston)
varImpPlot(bag.boston)

# Grow a random forest.
set.seed(1)
rf.boston <-randomForest(medv~., 
                         data=Boston, 
                         subset=train, 
                         mtry=6, 
                         importance=TRUE, 
                         n.tree = 5000)

# Predict.
yhat.rf <-predict(rf.boston, 
                  newdata=Boston[-train,])

# Calculate the test mean square of errors.
mean((yhat.rf-boston.test)^2)

library(gbm)
set.seed (1)
boost.boston = gbm( medv ~ ., data = Boston[train , ], distribution = "gaussian",
  n.trees = 5000, interaction.depth = 4
)
summary(boost.boston)

# Plot partial dependence plots for the two most important variables.
par(mfrow = c(1, 2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost = predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test) ^ 2)






