library(tidyverse)

data <- read_csv("BostonHousing.csv")
head(data)

# YOUR CODE HERE
hist(data$medv)

# YOUR CODE HERE
summary(data)

data$rad <- as.factor(data$rad)
data$chas <- as.factor(data$chas)
data$rad
data$chas

# YOUR CODE HERE
plot(data$crim, data$medv)
plot(data$zn, data$medv)
plot(data$indus, data$medv)
plot(data$nox, data$medv)
plot(data$rm, data$medv)
plot(data$dis, data$medv)
plot(data$tax, data$medv)
plot(data$ptratio, data$medv)
plot(data$lstat, data$medv)
plot(data$data$rad, data$medv)
plot(data$data$chas, data$medv)

# YOUR CODE HERE
par(mfrow=c(3,2))
boxplot(medv ~ data$chas, data = data)
boxplot(medv~ data$rad, data = data)

# YOUR CODE HERE
reg_model<- lm(medv~crim+zn+indus+nox+rm+age+dis+tax+ptratio+lstat+data$rad+data$chas,data=data)
summary(reg_model)
anova(reg_model)

# Plot the model using default plots
par(mfrow=c(2,2))
plot(reg_model)

# YOUR CODE HERE
predData <- data.frame(crim = c(3),

zn = c(11),

indus =c(11),

nox = c(0.5),

rm = c(6),

age = c(70),
                       
dis = c(3),

tax = c(300),

ptratio = c(20),

lstat =c(10))

# Predict the median value for the test cases
predmedv <- predict(reg_model,predData)
exp(predmedv)

##the code is showing errors due to the variable lengths for data$rad. 
## I tried making changes using chas and rad as it is and later as data$rad for the same. It doesn't seem to be working.

### Copy code into this cell to adjust variables

##increased the crime rate but retain businesses too while increasing nitrogen ocide. Wanted to understand worsct case locality value

# YOUR CODE HERE
predData <- data.frame(crim = c(7),

zn = c(11),

indus =c(4),

nox = c(1.5),

rm = c(6),

age = c(70),
                       
rad= c(4),
chas=c(1),
dis = c(3),

tax = c(300),

ptratio = c(20),

lstat =c(10))

# Predict the median value for the test cases
predmedv <- predict(reg_model,predData)
exp(predmedv)

##the code is showing errors due to the variable lengths for data$rad. 
## I tried making changes using chas and rad as it is and later as data$rad for the same. It doesn't seem to be working.

### Copy code into this cell to adjust variables

# YOUR CODE HERE
predData <- data.frame(crim = c(3),

zn = c(11),

indus =c(11),

nox = c(0.5),

rm = c(6),

age = c(70),
                       
rad= c(4),
chas=c(1),
dis = c(3),

tax = c(300),

ptratio = c(20),

lstat =c(10))

# Predict the median value for the test cases
predmedv <- predict(reg_model,predData)
exp(predmedv)

##the code is showing errors due to the variable lengths for data$rad. 
## I tried making changes using chas and rad as it is and later as data$rad for the same. It doesn't seem to be working.

### Copy code into this cell to adjust variables

##reduced the crime rate, increased retail businesses, reduced the pollution, and increased the number of rooms per dwelling
##tried best case scenario


# YOUR CODE HERE
predData <- data.frame(crim = c(1),

zn = c(22),

indus =c(20),

nox = c(0.2),

rm = c(10),

age = c(70),
                       
rad= c(4),
chas=c(1),
dis = c(3),

tax = c(300),

ptratio = c(20),

lstat =c(10))

# Predict the median value for the test cases
predmedv <- predict(reg_model,predData)
exp(predmedv)

##the code is showing errors due to the variable lengths for data$rad. 
## I tried making changes using chas and rad as it is and later as data$rad for the same. It doesn't seem to be working.

### Copy code into this cell to adjust variables

# YOUR CODE HERE
predData <- data.frame(crim = c(3),

zn = c(11),

indus =c(11),

nox = c(0.5),

rm = c(6),

age = c(70),
                       
rad= c(4),
chas=c(1),
dis = c(3),

tax = c(300),

ptratio = c(20),

lstat =c(10))

# Predict the median value for the test cases
predmedv <- predict(reg_model,predData)
exp(predmedv)

##the code is showing errors due to the variable lengths for data$rad. 
## I tried making changes using chas and rad as it is and later as data$rad for the same. It doesn't seem to be working.

# YOUR CODE HERE
predData <- data.frame(crim = c(3),

zn = c(11),

indus =c(11),

nox = c(0.5),

rm = c(6),

age = c(70),
                       
rad= c(4),
chas=c(1),
dis = c(3),

tax = c(300),

ptratio = c(20),

lstat =c(10))

# Predict the median value for the test cases
predmedv <- predict(reg_model,predData)
exp(predmedv)

##the code is showing errors due to the variable lengths for data$rad. 
## I tried making changes using chas and rad as it is and later as data$rad for the same. It doesn't seem to be working.


