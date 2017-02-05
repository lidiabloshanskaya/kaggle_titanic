library('tidyverse') # loads tons of packages
library('dplyr')
library('randomForest')
library('caret')


setwd("~/Dropbox/data science/kaggle - titanic")

train <- read.csv('train.csv', stringsAsFactors = FALSE,
                  strip.white = TRUE, na.strings = c("NA",""))
test <- read.csv('test.csv', stringsAsFactors = FALSE,
                  strip.white = TRUE, na.strings = c("NA",""))
attach(train)
table(train[,c("Survived", "Pclass")])
boxplot(Survived~Pclass,data=train, main="Survived vs class", 
        xlab="Class", ylab="Survived")
library(fields)
bplot.xy(Pclass, Survived)
bplot.xy(Age,Survived)
summary(train$Cabin)
names(train)
bplot.xy(Parch,Survived)
bplot.xy(SibSp,Survived)
bplot.xy(Fare,Survived)
bplot.xy(Sex,Survived)
# Convert Survived to Factor
train <- read.table("train.csv", sep = ",", header = TRUE)
train$Survived <- factor(train$Survived)
# Set a random seed (so you will get the same results as me)
set.seed(345)
# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +   
                 Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
               data = train, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation
          )
model
summary(test)
test$Survived<-predict(model,newdata=test)
test$Fare <- ifelse(is.na(test$Fare), mean(test$Fare, na.rm = TRUE), test$Fare)
test$Survived<-predict(model,newdata=test)

##linear regression
lm.train <- read.csv('train.csv', stringsAsFactors = FALSE,
                  strip.white = TRUE, na.strings = c("NA",""))
lm.test <- read.csv('test.csv', stringsAsFactors = FALSE,
                 strip.white = TRUE, na.strings = c("NA",""))
attach(lm.train)

lm.model<-lm(Survived ~ Pclass + Sex + SibSp +   
               Embarked + Parch + Fare,
             data=lm.train)
summary(lm.model)

glm.model<-glm(Survived ~ Pclass + Sex + SibSp +   
                 Embarked + Parch + Fare,
               family=binomial(link='logit'),data=lm.train)
summary(glm.model)

#library(e1071) ## machine learning and SVM's
#svm.model<-svm(, data=train)
