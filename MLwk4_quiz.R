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
table(pRF,vowel.test$y)
vowel.test$predRightRF <- pRF==vowel.test$y
tab <- table(vowel.test$predRight)
prop.table(tab)
# b) fit boosted predicter using "gbm" method (using train() in caret)
Boost <- train(y~.,data=vowel.train,method="gbm",verbose=FALSE)

pBoost <- predict(Boost,vowel.test)
vowel.test$predRightBoost <- pBoost==vowel.test$y
tab2 <- table(vowel.test$predRightBoost)
prop.table(tab2)
table(pRF,pBoost)

eqPred <- (vowel.test$predRightRF == vowel.test$predRightBoost)
tab3 <- table(eqPred)
prop.table(tab3)

#Incorrect answers: RF accuracy 0.9987
# correct answer: RF accuracy 0.6082, GBM accuracy 0.5152, agreemetn accuracy 0.6361

# Q2: loading Alzheimer's data
library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

training$diagnosis <- as.factor(training$diagnosis)
testing$diagnosis <- as.factor(testing$diagnosis)

RFad <- train(diagnosis ~ .,data=training,method="rf",prox=TRUE)
Boostad <- train(diagnosis~.,data=training,method="gbm",verbose=FALSE)
ldad <- train(diagnosis~.,data=training,method="lda")

predRF <- predict(RFad,training)
predBoost <- predict(Boostad,training)
predld <- predict(ldad,training)

predDf <- data.frame(predRF,predBoost,predld,diagnosis=training$diagnosis)

combRF <- train(diagnosis ~ .,data=predDf,method="rf",prox=TRUE)

predRF <- predict(RFad,testing)
predBoost <- predict(Boostad,testing)
predld <- predict(ldad,testing)

predDf <- data.frame(predRF,predBoost,predls,diagnosis=testing$diagnosis)
combRFpred <- predict(combRF,predDf)

testing$combRIGHT <- combRFpred==testing$diagnosis
testing$RFRIGHT <- predRF==testing$diagnosis
testing$boostRIGHT <- predBoost==testing$diagnosis
testing$ldRIGHT <- predld==testing$diagnosis

combRIGHT <- prop.table(table(testing$combRIGHT))
RFRIGHT <- prop.table(table(testing$RFRIGHT))
boostRIGHT <- prop.table(table(testing$boostRIGHT))
ldRIGHT <- prop.table(table(testing$ldRIGHT))

# Incorrect answers: 0.93 better than all three methods,0.76 better than lda but not other two, 


# Q3: concrete
set.seed(3523)
library(caret)
library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

lasso <- train(CompressiveStrength~.,data=training,method="lasso")
plot(lasso$finalModel, xvar="penalty", use.color=T)

# Correct answer: cement

# Q4

library(lubridate) # For year() function below

dat = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat

tstrain = ts(training$visitsTumblr)
tstest = ts(testing$visitsTumblr)

library(forecast)

bats <- bats(tstrain)
fcast <- forecast(bats,end=3000)
plot(fcast);lines(tstest,col="red")
accuracy(fcast,tstest)

#incorrect: 94%,

# Q5
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

library(e1071)
fit <- svm(CompressiveStrength ~ .,data=training)
fit_pred <- predict(fit,testing)
points(testing$CompressiveStrength,fit_pred,col="red",pch=4)

sqrt(mean((testing$CompressiveStrength - fit_pred)^2))

# correct answer: 6.72
