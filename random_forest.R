suppressWarnings(suppressMessages(library('tidyverse')))  # loads tons of packages
suppressWarnings(suppressMessages(library('dplyr')))
suppressWarnings(suppressMessages(library('randomForest')))
suppressWarnings(suppressMessages(library('caret')))
suppressWarnings(suppressMessages(library('ggthemes')))
suppressWarnings(suppressMessages(library('mice')))  ## imputations
suppressWarnings(suppressMessages(library('Amelia'))) ## stuff for missing values
setwd("~/Dropbox/data science/kaggle - titanic")



train <- read.csv('train.csv', stringsAsFactors = FALSE,
                     strip.white = TRUE, na.strings = c("NA",""))
test <- read.csv('test.csv', stringsAsFactors = FALSE,
                    strip.white = TRUE, na.strings = c("NA",""))

summary(train)
summary(test)

#set sex as factor
train$Sex <-as.factor(train$Sex)
test$Sex <-as.factor(test$Sex)
train$Embarked <-as.factor(train$Embarked)
test$Embarked <-as.factor(test$Embarked)

### MISSING VALUES: for Age <- mean

train$Age[is.na(train$Age)] <- -1 ##create totally different group not to mess with mean
test$Age[is.na(test$Age)] <- -1
test$Fare[is.na(test$Fare)]<- median(test$Fare,na.rm=T)
train$Embarked[is.na(train$Embarked)]="C"


## choosing features
features <- c("Pclass",
              "Age",
              "Sex",
              "Parch",
              "SibSp",
              "Fare",
              "Embarked")

### split the train set on train + CV set
set.seed(1)
splitIndex<-createDataPartition(train$PassengerId, p=.8, list = F, times=1)
head(splitIndex)
trainData<-train[splitIndex,]
CV<-train[-splitIndex,]


###### RANDOM FOREST #######
 rf.model<-train(Survived ~ Age+Pclass + Sex + SibSp +   
                   Parch + Fare + Embarked,
                 data=trainData,
                 method = "rf",
                 trControl = trainControl(method = "cv", # Use cross-validation
                                          number = 5) # Use 5 folds for cross-validation
                 )
rf.predict<-predict(rf.model,newdata=CV[,features])
summary(rf.predict)
rf.predict <- ifelse(rf.predict > 0.5,1,0)
misClasificError <- mean(rf.predict != CV$Survived)
print(paste('Accuracy',1-misClasificError)) ### gives 0.823
##whole set
rf.model<-train(Survived ~ Age+Pclass + Sex + SibSp +   
                  Parch + Fare + Embarked,
                data=train,
                method = "rf",
                trControl = trainControl(method = "cv", # Use cross-validation
                                         number = 5) # Use 5 folds for cross-validation
)
rf.predict<-predict(rf.model, test[,features])
rf.predict <- ifelse(rf.predict > 0.5,1,0)

submission<-data.frame(PassengerID = test$PassengerId, Survived = rf.predict)
write.csv(submission, file = 'rf_submission_01.csv', row.names = F)
