# Week 4 quiz

# Q1 
library(readr)
library(caret)
library(e1071)
library(gbm)
vowel.train <- read_csv("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/vowel.train")
vowel.test <- read_csv("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/vowel.test")

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

# a) fit random forest predictor relating y to remaining variables (using train() in caret)
RF <- train(y ~ .,data=vowel.train,method="rf",prox=TRUE)
print(RF)
pRF <- predict(RF,vowel.test)

# b) fit boosted predicter using "gbm" method (using train() in caret)
Boost <- train(y~.,data=vowel.train,method="gbm",verbose=FALSE)
predict(Boost,vowel.test)
pBoost <- predict(Boost,vowel.test)

table(pRF,pBoost)
equalPredictions <- (pRF == pBoost)

print(correctPredictions)
#Incorrect answers: RF accuracy 0.9987
