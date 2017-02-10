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

train$Age[is.na(train$Age)] <- mean(train$Age,na.rm=T)
test$Age[is.na(test$Age)] <- mean(test$Age,na.rm=T)
test$Fare[is.na(test$Fare)]<- median(test$Fare,na.rm=T)

### MISSING VALUES: age - PMM via mice

print(sum(is.na(train$Age)))
md.pattern(train)
missmap(train)
temp<-mice(train$Age,m=5,mazit=50,meth='pmm', seed=500)
summary(temp)
temp$imp$Age ##check the imputed data
imputeTrain<-complete(temp)
summary(imputeTrain)
##visualize the original and imputed data
par(mfrow=c(1,2)) # combine plots, also: layout(...)
hist(train$Age)
hist(imputeTrain$Age)

### MISSING VALUES: Embarked
missEmb.Id<-train$PassengerId[is.na(as.factor(train$Embarked))]
missEmb.Fare<-train$Fare[is.na(as.factor(train$Embarked))] ##output 80 and 80
missEmb.Pclass<-train$Pclass[is.na(as.factor(train$Embarked))] ## Pclass = 1 and 1 

mean(train$Fare[train$Pclass ==1 &
                        as.factor(train$Embarked)=="S"],na.rm=T)
mean(train$Fare[train$Pclass ==1 &
                        as.factor(train$Embarked)=="Q"],na.rm=T)
mean(train$Fare[train$Pclass ==1 &
                        as.factor(train$Embarked)=="C"],na.rm=T)

ggplot(train, aes(x = train$Embarked, y = train$Fare, fill = factor(train$Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  theme_few()

train$Embarked[is.na(train$Embarked)]="C"

##assign class 


summary(train)
summary(test)

##analyze predictors
AgeFactor<-cut(train$Age,c(0,5,10,18,25,30,40,50,60,70,80,90))
ggplot(train, aes(AgeFactor, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')+
  ggtitle("Survival vs Age")

ggplot(train, aes(train$Sex, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')+
  facet_wrap(~AgeFactor)+
  ggtitle("Survival vs Sex at different Ages")

ggplot(train, aes(train$Embarked, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')



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

###LOGISTIC REGRESSION########
glm.model<-glm(Survived ~ Age+Pclass + Sex + SibSp +   
                 Parch + Fare+Embarked,
               family=binomial(link = "logit"),data=trainData)
summary(glm.model)

glm.predict<-predict(glm.model, CV[,features],type='response')
summary(glm.predict)
glm.predict <- ifelse(glm.predict > 0.5,1,0)

misClasificError <- mean(glm.predict != CV$Survived)
print(paste('Accuracy',1-misClasificError))

##perform regression on the whole train set and create the submission 
glm.model<-glm(Survived ~ Age+Pclass + Sex + SibSp +   
                 Parch + Fare+Embarked,
               family=binomial(link='logit'),data=train)
glm.predict<-predict(glm.model, test[,features],type='response')
glm.predict <- ifelse(glm.predict > 0.5,1,0)

submission<-data.frame(PassengerID = test$PassengerId, Survived = glm.predict)
write.csv(submission, file = 'glm_submission.csv', row.names = F)


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
write.csv(submission, file = 'rf_submission.csv', row.names = F)
