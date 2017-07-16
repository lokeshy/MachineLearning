rm(list=ls(all=TRUE))

setwd('f:/ML/assignment-1-lokeshy/src/')
source('common.r')
str(bankdata)

set.seed(255)

train_RowIDs = sample(1:nrow(bankdata), nrow(bankdata)*0.6)
train = bankdata[train_RowIDs,]
test = bankdata[-train_RowIDs,]
mod_lm = glm(train$PersonalLoan ~ . , data = train , family = 'binomial')
summary(mod_lm)
print(mod_lm)
pridict_train = predict(object = mod_lm , newdata = train[,-16], type = 'response')
threshold = 0.25
pridict_train[pridict_train > threshold] = 1
pridict_train[pridict_train < threshold] = 0

buildConfusionMatrix(train$PersonalLoan , pridict_train)

pridict_test = predict(object = mod_lm , newdata = test[,-16], type = 'response')
pridict_test[pridict_test > threshold] = 1
pridict_test[pridict_test < threshold] = 0

buildConfusionMatrix(test$PersonalLoan , pridict_test)
