library(tidyverse)

data <- read.csv("Term Life Insurance.csv")
head(data)

hist(data$FACE)

hist(log(data$FACE))

data <- data %>% mutate(logFACE = log(FACE))

summary(data)

data <- data %>% filter(logFACE != -Inf)
summary(data)

# Plot raw and log-transformed FACE and INCOME variables
par(mfrow=c(1,2))
plot(data$INCOME, data$FACE); plot(log(data$INCOME), log(data$FACE))

# Use pair plots to visually evaluate the relationships between multiple variables
par(mfrow=c(1,1))
logINCOME = log(data$INCOME); 
logFACE = log(data$FACE);
NUMHH = data$NUMHH
EDUCATION = data$EDUCATION
pairs(data.frame(NUMHH, EDUCATION, logINCOME, logFACE))

cor(data.frame(NUMHH, EDUCATION, logINCOME, logFACE))

data <- data %>% mutate(logINCOME = log(INCOME), 
                        logTOTINCOME = log(TOTINCOME), 
                        logCHARITY = log(CHARITY),
                        logFACECVLIFEPOLICIES = log(FACECVLIFEPOLICIES), 
                        logCASHCVLIFEPOLICIES = log(CASHCVLIFEPOLICIES),
                        logBORROWCVLIFEPOL = log(BORROWCVLIFEPOL),
                        logNETVALUE = log(NETVALUE))

par(mfrow=c(3,2))
boxplot(logFACE ~ GENDER, data = data)
boxplot(logFACE ~ MARSTAT, data = data)
boxplot(logFACE ~ ETHNICITY, data = data)
boxplot(logFACE ~ SGENDER, data = data)
boxplot(logFACE ~ SMARSTAT, data = data)

reg_model <- lm(logFACE ~ logINCOME + EDUCATION + NUMHH, data = data)
summary(reg_model)

# Obtain the Anova table
anova(reg_model)

# Education
exp(0.206)

# Plot the model using default plots
par(mfrow=c(2,2))
plot(reg_model)

# Add test cases to a new data frame
predData <- data.frame(logINCOME = c(log(100000),log(110000),log(90000)), 
                       EDUCATION = c(16,18,15), 
                       NUMHH = c(5,6,3))

# Predict the balances for the test cases
predBalance <- predict(reg_model, predData)
exp(predBalance)

predData2 <- data.frame(logINCOME = log(100000), 
                       EDUCATION = 16, 
                       NUMHH = 5)
predBalance2 <- predict(reg_model, predData2)
exp(predBalance2)

predData2 <- data.frame(logINCOME = log(110000), 
                       EDUCATION = 12, 
                       NUMHH = 4)
predBalance2 <- predict(reg_model, predData2)
exp(predBalance2)


