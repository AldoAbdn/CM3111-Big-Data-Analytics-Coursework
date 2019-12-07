setwd("D:/RGU/3rdYear/Semester1/Big Data Analytics/Coursework/wd")
getwd()
#Test of homocide database 
dat = read.csv("data/homocides.csv")
#All records source are FBI, so null whole column
dat$Record.Source<-NULL
View(dat) #View data
summary(dat) #Data Summary
nrow(dat) #638454
ncol(dat) #23
sapply(dat,levels) #factors
levels(dat$Crime.Type)
library(caret)
library(randomForest)
#Partician set 
inTrain <- createDataPartition(y=dat$Perpetrator.Sex,p=.00001,list=FALSE)
training <- dat[inTrain,]
training <- subset(training,training$Perpetrator.Sex!="Unknown")
training <- training[!is.na(training$Perpetrator.Sex),]
training$Perpetrator.Sex <- factor(training$Perpetrator.Sex)
testing <- dat[-inTrain,]
testing <- testing[testing$Perpetrator.Sex!="Unknown",]
testing <- testing[!is.na(testing$Perpetrator.Sex),]
testing$Perpetrator.Sex <- factor(testing$Perpetrator.Sex)
testing<-testing[501:1500,]

factor(testing$Perpetrator.Sex)
levels(training$Perpetrator.Sex)
sapply(training,levels)
#Fit model
fitModel <- train(Perpetrator.Sex~.,data=training,tuneLength=1,method="rf",prox=TRUE)
varImp(fitModel)
#Getting and Visualising Class Centers

#Predicting new values
pred <- predict(fitModel, testing)
testing$correctPred <- pred==testing$Perpetrator.Sex
results <- table(pred,testing$Perpetrator.Sex)
sum(testing$correctPred)/nrow(testing)

factor(pred)

#Test of Credit Card db
dat = read.csv("data/creditcard.csv")
head(dat)
summary(dat)
sum(dat$Class==1)
View(cor(dat))
library(caret)
library(randomForest)
#Partician set 
inTrain <- createDataPartition(y=dat$Victim.Sex,p=.0001,list=FALSE)
training <- dat[inTrain,]
testing <- dat[-inTrain,]

#Fit model
fitModel <- train(Victim.Sex~.,data=training,method="rf",prox=TRUE)

#Getting and Visualising Class Centers

#Predicting new values
pred <- predict(fitModel, testing)
testing$correctPred <- pred==testing$Victim.Sex
results <- table(pred,testing$Victim.Sex)
results
testing
sum(testing$correctPred)/nrow(testing)
    