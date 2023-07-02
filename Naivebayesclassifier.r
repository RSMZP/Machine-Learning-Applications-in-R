# Load the required packages
library(tm)
library(e1071)
library(dplyr)
library(caret)

# Load the data
df <- read.csv("2_newsgroups.csv", stringsAsFactors = FALSE)

summary(df)

# Convert the title variable from character to factor
df$title <- as.factor(df$title)

# Create and inspect the corpus
corpus <- Corpus(VectorSource(df$text))
corpus
inspect(corpus[1])

# Clean the data
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

# Create the dtm
dtm <- DocumentTermMatrix(corpus.clean)

# Inspect the dtm
inspect(dtm[40:50, 10:15])

# Split the data into training and testing data using a 70%–30% split
set.seed(1)
train=sample(1:nrow(df), round(nrow(df)*.7))
df.train <- df[train,]
df.test <- df[-train,]
dtm.train <- dtm[train,]
dtm.test <- dtm[-train,]
corpus.clean.train <- corpus.clean[train]
corpus.clean.test <- corpus.clean[-train]
dim(dtm.train)

# Identify the most frequent terms that appear in at least five documents
freqwords <- findFreqTerms(dtm.train, 5)
length((freqwords))

# Use freqwords to build the dtm (train and test)
dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = freqwords))
dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = freqwords))
dim(dtm.test.nb)

# Convert counts
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("absent", "present"))

  y
}
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

# YOUR CODE HERE
classifier <- naiveBayes(trainNB, df.train$title, laplace = 1)

# YOUR CODE HERE
testpreds<-predict(classifier,newdata=testNB)
testpreds

# Display the confusion matrix

# YOUR CODE HERE

confusionMatrix(testpreds,testNB)


## I tried running this code multiple times with test datsets but none of them are giving results. 
## I am not sure if this will help me answer my questions

