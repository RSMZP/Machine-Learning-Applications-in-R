library('rpart')
library('rpart.plot')
library('caret')
library('ROCR')
library('tree')

# Load the data set
wine <- read.csv('winequality-white.csv', sep=';', stringsAsFactors=F)
# Review the data set
str(wine)
head(wine)

# Review the quality distribution
hist(wine$quality)

# Check for missing values
sum(is.na(wine))

# Create a quality factor variable where the quality is equal to or higher than 6
qF <- ifelse(wine$quality >= 6, "high", "low")
wine <- data.frame(wine, qF)
table(wine$qF)

# YOUR CODE HERE
# Create tree.
reg.tree <- tree(qF ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data = wine)

# Display the summary.
summary(reg.tree)

# Plot the tree with text descriptions.
plot(reg.tree)
text(reg.tree, pretty = 0, cex = 1)

# YOUR CODE HERE
plot(reg.tree)
text(reg.tree, pretty = 0, cex = 0.8)

# Split the data into training and testing data
set.seed(100)
train <-sample(1:nrow(wine), nrow(wine) / 2)
wine.test <- wine[-train,]
qF.test <-qF[-train]

# YOUR CODE HERE

# Create tree.
reg.tree <- tree(qF ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data = wine,subset=train)

# Display the summary.
summary(reg.tree)

# Plot the tree with text descriptions.
plot(reg.tree)
text(reg.tree, pretty = 0, cex = 1)

tree.pred <-predict(tree.winequality, wine.test, type="class")
table(tree.pred, qF.test)

# Misclassification error
mean(tree.pred!=qF.test)

# Perform cross-validation
set.seed(3)
cv.wine <-cv.tree(tree.winequality, FUN = prune.misclass)
names(cv.wine)
cv.wine

# Plot the output using graphs
par(mfrow=c(1,2))
plot(cv.wine$size, cv.wine$dev, type="b")
plot(cv.wine$k, cv.wine$dev, type="b")

# Plot the pruned tree
par(mfrow = c(1,1))
prune.wine <- prune.misclass(tree.winequality, best=4)
# YOUR CODE HERE

plot(prune.wine)
text(prune.wine, pretty=0)


# Compute the test error rate using the pruned tree 
tree.pred <-predict(prune.wine, wine.test, type="class")
table(tree.pred,qF.test)
mean(tree.pred!=qF.test)
