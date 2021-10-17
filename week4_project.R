# Week four project

training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

# Installing libraries
library(caret)
library(rpart)
library(rattle)

# Removing near zero variance variables
nzv <- nearZeroVar(training)

train_dat <- training[,-nzv]
test_dat <- testing[,-nzv]

# Classifying factor variables
train_dat$user_name <- as.factor(train_dat$user_name)
train_dat$classe <- as.factor(train_dat$classe)

test_dat$user_name <- as.factor(test_dat$user_name)

# Removing variables with at least 50% NA
train_dat <- train_dat[,which(colMeans(!is.na(train_dat))>0.95)]

test_dat <- test_dat[,which(colMeans(!is.na(test_dat))>0.95)]

# Removing x variable
train_dat <- train_dat[,-(1:5)]
test_dat <- test_dat[,-(1:5)]

# Cross-validation

inTrain <- createDataPartition(y=train_dat$classe,p=0.6,list=FALSE)

t_subtrain <- train_dat[inTrain,]
t_subtest <- train_dat[-inTrain,]

## Decision tree
treeFit <- train(classe ~ .,data=t_subtrain,method="rpart")
print(treeFit$finalModel)

library(rattle)
fancyRpartPlot(treeFit$finalModel)

pred_tree <- predict(treeFit,newdata=t_subtest)
t_subtest$pred_tree <- pred_tree

confusionMatrix(t_subtest$classe,t_subtest$pred_tree)

## Random forest
library(randomForest)
rfFit <- randomForest(y=t_subtrain[,54],x=t_subtrain[,1:53])

predRF <- as.factor(predict(rfFit,newdata=t_subtest))
t_subtest$classe <- as.factor(t_subtest$classe)

confusionMatrix(t_subtest$classe,predRF)
varImp(rfFit)


## 
