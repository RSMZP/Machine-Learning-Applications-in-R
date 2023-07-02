# YOUR CODE HERE
data<- read.csv("Wine quality.csv")

str(data)


# YOUR CODE HERE
library(caret)
library(dplyr)

y <- data$wine
y <- as.factor(y)
x <- data %>% select(-wine)

# YOUR CODE HERE
knn_model1 <- knn3(wine ~., data = data, k = 5)
knn_pred1 <- predict(knn_model1, data[, -13], type = "class")
confusionMatrix(knn_pred1, data$wine)

knn_model <- knn3(wine ~., data = data, k = 3)
knn_pred <- predict(knn_model, data[, -13], type = "class")
confusionMatrix(knn_pred, data$wine)

hyper_grid <- expand.grid(k = seq(3, 25, by = 2))

# YOUR CODE HERE
ctrl<- trainControl(method="LGOCV",
       p=-0.7,
       number=1,
       savePredictions=TRUE)

# YOUR CODE HERE
source("KNN Function.R") ##i tried using this one later but preprocessing never worked for me
data_train_x<- datapreprocessing(x) ##this function is not working for me
data_train_y<-datapreprocessing(y)

knn_models<-train(
    data_train_x,
    data_train_y,
    method="knn",
    tuneGrid= hyper_grid,
    # preProc- = c("center", "scale"),
    trControl=ctrl
)

data <- read.csv("Combined cycle power plant.csv")

library(ISLR)
str(data)

PE <- data$PE
V <- data$V

# YOUR CODE HERE
plot(PE,V)

# YOUR CODE HERE
library(ISLR)
poly_v<-poly(V,3)

poly_reg <- lm(PE ~ poly_v)
summary(poly_reg)

# YOUR CODE HERE
poly_v<-poly(V,10)
poly_reg<- lm(PE~poly_v)
summary(poly_reg)

library(caret)

# YOUR CODE HERE
fitControl<- trainControl(method="repeatedcv",
                         number= 5,
                         repeats=2)

R2 <- c()

for (d in 1:10) {
    f <- bquote(PE ~ poly(V, .(d)))
    LinearRegressor <- caret::train(as.formula(f), data = data, method = "lm",trControl = fitControl)
    R2 <- c(R2, LinearRegressor$results$Rsquared)
} 

# YOUR CODE HERE
print(R2)
plot(R2,type="1")

