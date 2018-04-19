rm(list=ls(all=TRUE))
library(vegan)
library(readxl)
library(car)
library(C50)
library(rpart)
library(rpart.plot)	
library(class)
library(dummies)
library(plyr)
library(data.table)
library(randomForest)
library(e1071)

setwd('f:/ML/Project/Target_Marketing_and_cross_selling__Data')
source('common.r')

data  = read_excel("Target Marketing and cross selling - Data.xls", 1)

## Renames columns for ease of coding
setnames(data, old =  c('Branch ID','Customer ID','Bill To','Customer Type','Customer Name','Zip Code','Zip 5','Area 1','Area 2','Setup Date',
                        'Last Service Date','Ticket Number','Current Email','Rev Code','Job Code','Call Date','Complete Date','Ticket Revenue','Year Month',
                        'Week Ending Date','Call Time','Schedule Date','Schedule Time','Dispatch Date','Dispatch Time','Complete Time'),
              new = c('Branch.ID','Customer.ID','Bill.To','Customer.Type','Customer.Name','Zip.Code','Zip.5','Area.1','Area.2','Setup.Date',
                      'Last.Service.Date','Ticket.Number','Current.Email','Rev.Code','Job.Code','Call.Date','Complete.Date','Ticket.Revenue','Year.Month',
                      'Week.Ending.Date','Call.Time','Schedule.Date','Schedule.Time','Dispatch.Date','Dispatch.Time','Complete.Time'))



### Drop attr unnecessay cols ######
data = subset(data,select = -c(Branch.ID,Customer.Name,Bill.To,Current.Email,Ticket.Number))
### Drop location attributes #############
data = subset(data,select = -c(Address,City,Zip.Code ,Zip.5,Area.1,Area.2,Contact))


summary(data)
str(data)

###### Feature Generation ################

data$Call.Date.Time = data$Call.Date + data$Call.Time

data$Dispatch.Date.Time = data$Dispatch.Date + data$Dispatch.Time

data$Schedule.Date.Time = data$Schedule.Date + data$Schedule.Time

data$ScheduleTime.DispatchTime.Diff = as.numeric(as.Date(data$Dispatch.Date.Time) - as.Date(data$Schedule.Date.Time))

data$DispatchTime.CallTime.Diff = as.numeric(as.Date(data$Dispatch.Date.Time) - as.Date(data$Call.Date.Time))

data$ScheduleTime.CallTime.Diff = as.numeric(as.Date(data$Schedule.Date.Time) - as.Date(data$Call.Date.Time))

str(data)

#Group all the customer ids together in ordered way.

dfById = arrange(data,data$Customer.ID,data$Call.Date)
summary(dfById)
str(dfById)

#Check the number of times each customer gets back. If a customer doesn't has enough historical data we drop that customer. Threshold -> Number of Transactions for a customer must not be less than 3.
cust_frequency = table(dfById$Customer.ID)
cust_frequency = as.data.frame(cust_frequency)
str(cust_frequency)
summary(cust_frequency)

uniqueCustomers = unique(dfById$Customer.ID)
dfById$Number.Days.NextCall = NumberOfDaysBetweenCalls(dfById,uniqueCustomers)


avgDays = tapply(dfById$Number.Days.NextCall, dfById$Customer.ID, mean)


dfById$Average.Return.Days = MeanCustomerReturnDays(avgDays,uniqueCustomers,dfById)

str(dfById)


dfById$Dispatched.Before.Scheduled.Date = (dfById$Dispatch.Date <= dfById$Schedule.Date)

dfById$Completed.Before.Scheduled.Date = dfById$Complete.Date <= dfById$Schedule.Date

dfById$Service.Complete.Days = as.numeric(as.Date(dfById$Complete.Date) - as.Date(dfById$Call.Date))

mean(dfById$Service.Complete.Days)

dfById$Completed.Within.Mean.CompletedDays = ifelse(dfById$Service.Complete.Days > ceiling(mean(dfById$Service.Complete.Days)), FALSE, TRUE)



summary(dfById)



#Drop the first record as its not of any help for us
dfById = subset(x = dfById, dfById$Number.Days.NextCall != 0)

dfById$Churn = ((dfById$Number.Days.NextCall > dfById$Average.Return.Days) | (dfById$Dispatched.Before.Scheduled.Date == FALSE)
                             | (dfById$Completed.Before.Scheduled.Date == FALSE)
                             | (dfById$Completed.Within.Mean.CompletedDays== FALSE))


dfById$Rev.Code.For.Non.Churn = ifelse(dfById$Churn == TRUE,0,dfById$Rev.Code)

uniqueStates = unique(dfById$State)



dfById$State.Churn.Percentage = ChurnPercentageStateWise(uniqueStates , dfById)


str(dfById)


####################### Data Preprocessing for model building #####################

###### Pick Generated Features only for model Building #######
customer_id = dfById$Customer.ID

imp_variables = c('Customer.Type','State','Rev.Code' ,'Job.Code','Ticket.Revenue',
                  'ScheduleTime.DispatchTime.Diff','DispatchTime.CallTime.Diff','ScheduleTime.CallTime.Diff',
                  'Number.Days.NextCall','Average.Return.Days','Dispatched.Before.Scheduled.Date',
                  'Completed.Before.Scheduled.Date','Service.Complete.Days','Completed.Within.Mean.CompletedDays',
                  'State.Churn.Percentage','Churn','Rev.Code.For.Non.Churn')

df_for_model = subset(dfById , select = imp_variables)


str(df_for_model)

cat_attr =  c('Customer.Type' ,'State', 'Rev.Code' , 'Job.Code','Dispatched.Before.Scheduled.Date','Completed.Before.Scheduled.Date','Completed.Within.Mean.CompletedDays','Churn','Rev.Code.For.Non.Churn')
num_attr = setdiff(colnames(df_for_model),cat_attr) 

cat_data = data.frame(sapply(df_for_model[,cat_attr], as.factor))
num_data = data.frame(sapply(df_for_model[,num_attr],as.numeric))
num_data = decostand(num_data, "range") 

df = cbind(num_data,cat_data)
df$Customer.ID = customer_id
str(df)

DetectOutliers(df,df$Service.Complete.Days)
DetectOutliers(df,df$Ticket.Revenue)
sum(is.na(df))


############## divide data into test and train ##############
test_row = GetTestRows(df,uniqueCustomers)
train_row <- setdiff(rownames(df), test_row)
df$Customer.ID = NULL
#Extract test and train data
dfTestChurn = df[test_row,]
dfTrainChurn = df[train_row,]

dfTestService = dfTestChurn
dfTrainService = dfTrainChurn

dfTestChurn$Rev.Code.For.Non.Churn = NULL
dfTrainChurn$Rev.Code.For.Non.Churn = NULL



sum(is.na(dfTestChurn))
sum(is.na(dfTrainChurn))
########## Build Model For Churn###################
#logistic regression model
mod_lm <- glm (Churn ~ ., data = dfTrainChurn, family = binomial,control = list(maxit = 100))
summary(mod_lm)

vif(mod_lm)

ld.vars <- attributes(alias(mod_lm)$Complete)$dimnames[[1]]
ld.vars
class(ld.vars)

#### Drop "ScheduleTime.CallTime.Diff" and build model ###########
dispCallDiff = dfTrainChurn$DispatchTime.CallTime.Diff 
dfTrainChurn$DispatchTime.CallTime.Diff = NULL

mod_lm2 <- glm (Churn ~ ., data = dfTrainChurn, family = binomial,control = list(maxit = 100))
summary(mod_lm2)
vif(mod_lm2)
print(mod_lm2)
pridict_train = predict(object = mod_lm2 , newdata = subset(x = dfTrainChurn, select = -c(Churn), type = 'response'))

threshold = 0.50
pridict_train[pridict_train > threshold] = 1
pridict_train[pridict_train < threshold] = 0

BuildConfusionMatrix(dfTrainChurn$Churn , pridict_train)


pridict_test = predict(object = mod_lm2 , newdata = subset(x = dfTestChurn, select = -c(Churn), type = 'response'))
pridict_test[pridict_test > threshold] = 1
pridict_test[pridict_test < threshold] = 0
BuildConfusionMatrix(dfTestChurn$Churn , pridict_test)

dfTrainChurn$DispatchTime.CallTime.Diff = dispCallDiff

##### Decision Tree Cart for churn############
cartTree = rpart(Churn~.,data = dfTrainChurn ,method = 'class')
printcp(cartTree)

# Plot the tree						
rpart.plot(cartTree,fallen.leaves = TRUE)

### predict ####
prediction = predict(cartTree, newdata=dfTestChurn, type="class")
### calculate recall , accuracy , precisson ####
BuildConfusionMatrix(dfTestChurn$Churn,prediction)

###### Decision Tree C50 for churn#########

C50Tree = C5.0(Churn~.,data = dfTrainChurn , rules=TRUE)
summary(C50Tree)
C5imp(C50Tree, pct=TRUE)
# Plot the tree						
pred = predict(C50Tree, newdata = dfTestChurn , type="class")

BuildConfusionMatrix(dfTestChurn$Churn, pred)

############ KNN for churn######################

dfTrainChurnCp = dfTrainChurn
dfTestChurnCp = dfTestChurn

str(dfTrainChurnCp)
str(dfTestChurnCp)
#### Convert categorical to Numerical as KNN only works in numerical ##########
dfTrainChurnCp$Dispatched.Before.Scheduled.Date = ifelse(dfTrainChurnCp$Dispatched.Before.Scheduled.Date == TRUE , 1, 0) 
dfTrainChurnCp$Completed.Before.Scheduled.Date = ifelse(dfTrainChurnCp$Completed.Before.Scheduled.Date == TRUE , 1, 0) 
dfTrainChurnCp$Completed.Within.Mean.CompletedDays = ifelse(dfTrainChurnCp$Completed.Within.Mean.CompletedDays == TRUE , 1, 0) 
dfTrainChurnCp$Churn = ifelse(dfTrainChurnCp$Churn == TRUE , 1, 0) 
dfTrainChurnCp$Rev.Code = as.numeric(dfTrainChurnCp$Rev.Code)
dfTrainChurnCp$Job.Code = as.numeric(dfTrainChurnCp$Job.Code)


dfTestChurnCp$Dispatched.Before.Scheduled.Date = ifelse(dfTestChurnCp$Dispatched.Before.Scheduled.Date == TRUE , 1, 0) 
dfTestChurnCp$Completed.Before.Scheduled.Date = ifelse(dfTestChurnCp$Completed.Before.Scheduled.Date == TRUE , 1, 0) 
dfTestChurnCp$Completed.Within.Mean.CompletedDays = ifelse(dfTestChurnCp$Completed.Within.Mean.CompletedDays == TRUE , 1, 0) 
dfTestChurnCp$Churn = ifelse(dfTestChurnCp$Churn == TRUE , 1, 0) 
dfTestChurnCp$Rev.Code = as.numeric(dfTestChurnCp$Rev.Code)
dfTestChurnCp$Job.Code = as.numeric(dfTestChurnCp$Job.Code)


customerType = dummy(dfTrainChurnCp$Customer.Type)
state = dummy(dfTrainChurnCp$State)

dfTrainChurnCp$Customer.Type = NULL
dfTrainChurnCp$State = NULL
dfTrainChurnCp = cbind(dfTrainChurnCp,customerType, state)

customerType = dummy(dfTestChurnCp$Customer.Type)
state = dummy(dfTestChurnCp$State)

dfTestChurnCp$Customer.Type = NULL
dfTestChurnCp$State = NULL
dfTestChurnCp = cbind(dfTestChurnCp,customerType, state)


predKnn=knn(train = dfTrainChurnCp, test = dfTestChurnCp, dfTrainChurnCp$Churn, k = 1, prob=TRUE)

BuildConfusionMatrix(dfTestChurnCp$Churn , predKnn)

########## KNN Model for Churn End #######################

######### Model building for Service Type Prediction #######################

str(dfTestService)
str(dfTrainService)

#### Remove Churn Variable #########
dfTestService$Churn = NULL
dfTrainService$Churn = NULL

######### Decision Tree for Revenue Code##############

##### Decision Tree C5.0 for churn############
tree = C5.0(Rev.Code.For.Non.Churn~.,data = dfTrainService ,method = 'class')
summary(tree)
C5imp(tree, pct=TRUE)

### predict ####
prediction = predict(tree, newdata=dfTestService, type="class")
### calculate recall , accuracy , precisson ####
BuildConfusionMatrix(dfTestService$Rev.Code.For.Non.Churn,prediction)

######## Random Forest ################
model = randomForest(Rev.Code.For.Non.Churn ~ ., data=dfTrainService, 
                     keep.forest=TRUE, ntree=50) 
# Print and understand the model
print(model)
model$importance  
pred_Test = predict(model, 
                     dfTestService[,setdiff(names(dfTestService), "Rev.Code.For.Non.Churn")],
                     type="response", 
                     norm.votes=TRUE)
BuildConfusionMatrix(dfTestService$Rev.Code.For.Non.Churn,pred_Test)

##### Build Best Model with Top Attribute based i mean decrese gini drop anyting below 70##### 

best_model = randomForest(Rev.Code.For.Non.Churn~Ticket.Revenue+Number.Days.NextCall+
                            Average.Return.Days+Average.Return.Days+State+Rev.Code+Job.Code+Service.Complete.Days+Completed.Within.Mean.CompletedDays,
                            data = dfTrainService,
                           keep.forest=TRUE, ntree=50)


pred_Test = predict(best_model, 
                    dfTestService[,setdiff(names(dfTestService), "Rev.Code.For.Non.Churn")],
                    type="response", 
                    norm.votes=TRUE)
BuildConfusionMatrix(dfTestService$Rev.Code.For.Non.Churn,pred_Test)

##### SVM #####################
dfTrainService$Dispatched.Before.Scheduled.Date = ifelse(dfTrainService$Dispatched.Before.Scheduled.Date == TRUE , 1, 0) 
dfTrainService$Completed.Before.Scheduled.Date = ifelse(dfTrainService$Completed.Before.Scheduled.Date == TRUE , 1, 0) 
dfTrainService$Completed.Within.Mean.CompletedDays = ifelse(dfTrainService$Completed.Within.Mean.CompletedDays == TRUE , 1, 0) 
dfTrainService$Rev.Code = as.numeric(dfTrainService$Rev.Code)
dfTrainService$Job.Code = as.numeric(dfTrainService$Job.Code)
dfTrainService$Rev.Code.For.Non.Churn = as.numeric(dfTrainService$Rev.Code.For.Non.Churn)


dfTestService$Dispatched.Before.Scheduled.Date = ifelse(dfTestService$Dispatched.Before.Scheduled.Date == TRUE , 1, 0) 
dfTestService$Completed.Before.Scheduled.Date = ifelse(dfTestService$Completed.Before.Scheduled.Date == TRUE , 1, 0) 
dfTestService$Completed.Within.Mean.CompletedDays = ifelse(dfTestService$Completed.Within.Mean.CompletedDays == TRUE , 1, 0) 
dfTestService$Rev.Code.For.Non.Churn = ifelse(dfTestService$Rev.Code.For.Non.Churn == TRUE , 1, 0) 
dfTestService$Rev.Code = as.numeric(dfTestService$Rev.Code)
dfTestService$Job.Code = as.numeric(dfTestService$Job.Code)

customerType = dummy(dfTrainService$Customer.Type)
state = dummy(dfTrainService$State)

dfTrainService$Customer.Type = NULL
dfTrainService$State = NULL
dfTrainService = cbind(dfTrainService,customerType, state)

customerType = dummy(dfTestService$Customer.Type)
state = dummy(dfTestService$State)

dfTestService$Customer.Type = NULL
dfTestService$State = NULL
dfTestService = cbind(dfTestService,customerType, state)


TrainDataForSVM = subset(x = dfTrainService, select = -c(Rev.Code.For.Non.Churn))
svm_model = svm(x = TrainDataForSVM, 
            y = dfTrainService$Rev.Code.For.Non.Churn, 
            type = "C-classification", 
            kernel = "linear", cost = 10, gamma = 0.25) 
# Look at the model summary
summary(svm_model)

svm_model$index

plot(cmdscale(dist(TrainDataForSVM)),
     col = as.integer(dfTrainService$Rev.Code.For.Non.Churn),
     pch = c("o","+")[1:nrow(dfTrainService) %in% svm_model$index + 1])
# Predict on test data
TestDataForSVM = subset(x = dfTestService, select = -c(Rev.Code.For.Non.Churn))
pred_Test_svm = predict(svm_model, TestDataForSVM) 
#BuildConfusionMatrix(dfTestService$Rev.Code.For.Non.Churn,pred_Test_svm)
conf_matrix = table(dfTestService$Rev.Code.For.Non.Churn,pred_Test_svm)


################





















