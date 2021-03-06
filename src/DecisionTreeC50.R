rm(list=ls(all=TRUE))
setwd('f:/ML/assignment-1-lokeshy/src/')
source('common.r')
## loan is target variable check class imbalance if any##

checkClassImbalance(bankdata$PersonalLoan)

str(bankdata)
##### Deal with missing Values ####
sum(is.na(bankdata))

## check for outliers

detectOutliers(bankdata , bankdata$CCAvg)
detectOutliers(bankdata , bankdata$Income)

plot(x = bankdata$Income , y=bankdata$PersonalLoan)
plot(x = bankdata$Age , y=bankdata$PersonalLoan)
plot(x = bankdata$Education , y=bankdata$PersonalLoan)

### divide test and train  60 , 40######
set.seed(123)
dt = sort(sample(nrow(bankdata), nrow(bankdata)*.6))
train<-bankdata[dt,]
test<-bankdata[-dt,]
rm(dt)

### check % distribution of target variable in test and train#### 
checkClassImbalance(bankdata$PersonalLoan)
checkClassImbalance(train$PersonalLoan)
checkClassImbalance(test$PersonalLoan)

#### Cart Tree Classification ####

library(C50)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)	

tree = C5.0(PersonalLoan~.,data = train , rules=TRUE)
summary(tree)
C5imp(tree, pct=TRUE)

prediction = predict(tree, newdata = test , type="class")

buildConfusionMatrix(test$PersonalLoan, prediction)









