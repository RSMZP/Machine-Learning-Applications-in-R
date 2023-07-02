library(ISLR)
library(tree)

hitters <- Hitters
hitters = na.omit(hitters)
head(hitters)

# Set the seed for reproducibility.
set.seed(1234)

# Create the training and testing data sets.
train <-  sample(nrow(hitters), nrow(hitters) / 2)
hitters.train <-  hitters[train, ]
hitters.test <-  hitters[-train, ]

plot(hitters$Years,hitters$Hits)

# Create tree.
reg.tree <- tree(Salary ~ Years + Hits, data = hitters)

# Display the summary.
summary(reg.tree)

# Plot the tree with text descriptions.
plot(reg.tree)
text(reg.tree, pretty = 0, cex = 1)

reg.tree

# Create a more complex tree model.
tree.baseball <- tree(Salary ~ Hits + HmRun + Runs + RBI + Walks + Years + Errors, 
                       data = hitters.train)

# Display the summary data of our model.
summary(tree.baseball)

# Plot the new tree with labels.
plot(tree.baseball)
text(tree.baseball, pretty = 0, cex = 0.8)

# Display text overview.
tree.baseball

# Create new object 'prune1.baseball' where we remove one node from the tree object and print the summary.
prune1.baseball=prune.tree(tree.baseball,best=7)
summary(prune1.baseball)

# Plot with labels.
plot(prune1.baseball)
text(prune1.baseball, pretty = 0, cex = 0.8)

# Display text overview.
prune1.baseball

# Use cv.tree to perform cross-validation.
cv.baseball <- cv.tree(tree.baseball)
plot(cv.baseball$size,cv.baseball$dev,type='b',xlab = "Tree Size", ylab = "CV-MSE")

names(cv.baseball)
cv.baseball

# Prune the tree, display pruned tree.
prune.baseball <- prune.tree(tree.baseball,best=5)
plot(prune.baseball)
text(prune.baseball,pretty=0)

# Compute the test error rate using the unpruned tree.
yhat_unpruned <- predict(tree.baseball, newdata = hitters[-train,])
# True values on the testing data.
t2 <- hitters[-train, "Salary"]
# Compute the test MSE unpruned tree.
mean((yhat_unpruned - t2) ^ 2)

# Compute the test error rate using the pruned tree. 
yhat_pruned <- predict(prune.baseball, newdata = hitters[-train,])
# True values on the testing data.
t3 <- hitters[-train, "Salary"]
# Compute the test MSE pruned tree.
mean((yhat_pruned - t3) ^ 2)

# Display the summary values of salary to determine a suitable "high" threshold.
summary(hitters$Salary)

# Create the new variable.
High <- ifelse(hitters$Salary < 750, "No", "Yes")

# Merge the new variable with the data.
hitters <- data.frame(hitters, High)

# Create a classification tree.
ctree.baseball <- tree(High ~. -Salary, data = hitters)

# Print the tree summary.
summary(ctree.baseball)

# Plot and label the tree.
plot(ctree.baseball)
text(ctree.baseball, pretty = 0, cex = 1)

# Print the tree information.
ctree.baseball

# Recreate training and testing data sets.
set.seed(9005)
train <-  sample(nrow(hitters), nrow(hitters) / 2)
hitters.train <-  hitters[train, ]
hitters.test <-  hitters[-train, ]
High.test <- High[-train]

# Create tree using training data.
ctree.baseball <- tree(High ~ . -Salary, data = hitters, subset=train)

# Predict the class on the test data.
ctree.pred <-predict(ctree.baseball, hitters.test, type="class")

# Print the confusion matrix and test error rate.
table(ctree.pred, High.test)
mean(ctree.pred!=High.test)

# Perform cross-validation to determine optimal tree complexity.
cv.cbaseball <-cv.tree(ctree.baseball, FUN = prune.misclass)

# Print summary values.
names(cv.cbaseball)
cv.cbaseball

par(mfrow=c(1,2))
plot(cv.cbaseball$size, cv.cbaseball$dev, type="b")
plot(cv.cbaseball$k, cv.cbaseball$dev, type="b")

# Index of tree with minimum error.
min.idx <- which.min(cv.cbaseball$dev)
min.idx

# Number of terminal nodes in that tree.
min.idx.tn <- cv.cbaseball$size[min.idx]
min.idx.tn

# Prune the tree to be the specified number of terminal nodes.
prune.baseball <- prune.misclass(ctree.baseball, best=3)

# Plot the pruned tree.
par(mfrow=c(1,1))
plot(prune.baseball)
text(prune.baseball, pretty=0)

# Compute the test error rate using the pruned tree. 
ctree2.pred <-predict(prune.baseball, hitters.test, type="class")

# Print the confusion matrix and test error rate.
table(ctree2.pred,High.test)
mean(ctree2.pred!=High.test)


