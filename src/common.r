#### file with common utility function and operations ###
#rm(list=ls(all=TRUE))
library(vegan)
detectOutliers = function(data_frame, feature) {
  col_name = eval(substitute(feature),eval(data_frame))
  tot = sum(!is.na(col_name))
  na1 = sum(is.na(col_name))
  mean_actual = mean(col_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(col_name, main="With outliers")
  hist(col_name, main="With outliers", xlab=NA, ylab=NA)
  outlier = boxplot.stats(col_name)$out
  mo = mean(outlier)
  col_name = ifelse(col_name %in% outlier, NA, col_name)
  boxplot(col_name, main="Without outliers")
  hist(col_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 = sum(is.na(col_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  mean_nil_outlier = mean(col_name, na.rm = T)
  message("Mean without removing outliers: ", mean_actual)
  message("Mean if we remove outliers: ", mean_nil_outlier)
}

checkClassImbalance = function(target){
  class_Table = table(target)
  df = as.data.frame(class_Table)
  for(i in  1:nrow(df)){
    row <- df[i,]
    message("class : " , row[1] , " freq : " , row[2] , " percentage : " , round((row[2]/length(target))*100,4) ,"%")
  }
  return(class_Table) 
}

setwd('f:/ML/MachineLearning/src/')
total_attr = c('ID','Age','Experience','Income','ZIPCode','Family','CCAvg','Education',
               'Mortgage','PersonalLoan','SecuritiesAccount','CDAccount','Online','CreditCard')

bankdata = read.csv(file = 'UnivBank.csv' , col.names = total_attr)

summary(bankdata)

# drop zipcode and ID 

bankdata = subset(bankdata , select = -c(ID ,ZIPCode,Experience))
summary(bankdata)

### Convert variables to propper types 

str(bankdata)
bankdata = data.frame(sapply(bankdata, as.character))
num_attr = c('Age','Income','CCAvg','Mortgage')
cat_attr = setdiff(colnames(bankdata),num_attr)

cat_data = data.frame(sapply(bankdata[,cat_attr], as.factor))
num_data = data.frame(sapply(bankdata[,num_attr],as.numeric))

num_data = decostand(num_data, "range") 

bankdata = cbind(num_data,cat_data)
str(bankdata)
rm(num_attr,cat_attr,num_data,cat_data)

buildConfusionMatrix = function(actualData, prediction){
  conf_matrix = table(actualData,prediction)
  acc = sum(diag(conf_matrix))/ nrow(test)
  rec = (conf_matrix[2,2])/(conf_matrix[2,1]+conf_matrix[2,2])
  pre = conf_matrix[2,2]/sum(conf_matrix[,2])
  print(c("accuracy"=acc,"recall"=rec,"precision"=pre))
}


### Do PCA of Num data 

#### Take numerical attr out #### 

bankdataNum = subset(bankdata , select = c(Age,Income,CCAvg,Mortgage))

cor(bankdataNum[,-1])
summary(bankdataNum)

apply(bankdataNum , 2 , mean)
apply(bankdataNum , 2 , var)

### Normalize values 
library(vegan)
bankdataNum = decostand(bankdataNum,method = "range")

apply(bankdataNum , 2 , mean)
apply(bankdataNum , 2 , var)

pca_data = princomp(bankdataNum)
summary(pca_data)
plot(pca_data)
print(pca_data)
pca_data$loadings[,]

linearFeatures<-data.frame(pca_data$scores[,1:4])

cor(bankdataNum)
pca_data <- prcomp(bankdataNum)
summary(pca_data)
plot(pca_data)
print(pca_data)
biplot (pca_data , scale =0)

rm(bankdataNum)


############   Autoencoders to generate NonLinear Features ################
#Extract features using autoencoder method
library(h2o)
h2o.init(ip='localhost', port=54321, max_mem_size = '1g')

# Import a local R train data frame to the H2O cloud
bank.hex <- as.h2o(x = bankdata, destination_frame = "bank.hex")
y = "PersonalLoan"
x = setdiff(colnames(bank.hex), y)

autoenc = h2o.deeplearning(x = x, autoencoder = T,
                           training_frame = bank.hex,
                           activation = "Tanh",
                           hidden = c(20), epochs = 100)

autoEncoderFeatures = as.data.frame(h2o.deepfeatures(data = bank.hex[,x], 
                                                 object = autoenc))
#h2o.shutdown(prompt = FALSE)
############### Extract top attr from RandomForest ############
bankdataOriginal = bankdata
bankdata = data.frame(bankdataOriginal,linearFeatures,autoEncoderFeatures)

str(bankdata)

library(randomForest)
rf_DL <- randomForest(PersonalLoan ~ ., data=bankdata, 
                      keep.forest=TRUE, ntree=30)
round(importance(rf_DL), 2)
plot(rf_DL$importance)

importanceValues = data.frame(attribute=rownames(round(importance(rf_DL), 2)),
                              MeanDecreaseGini=round(importance(rf_DL), 2))
importanceValues = importanceValues[order(-importanceValues$MeanDecreaseGini),]

# Top  Important attributes
TopImpAttrs = as.character(importanceValues$attribute[1:15])

TopImpAttrs

Target = bankdata$PersonalLoan
bankdata_imp = subset(bankdata , select = TopImpAttrs)
bankdata_imp$PersonalLoan = Target
#bankdata =  data.frame(bankdata_imp,linearFeatures,autoEncoderFeatures)

str(bankdata_imp)
bankdata = data.frame(bankdata_imp)

rm(x,y,TopImpAttrs,Target,total_attr,importanceValues,autoenc,bank.hex,pca_data,rf_DL,bankdata_imp)




