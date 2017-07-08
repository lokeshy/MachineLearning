#### file with common utility function and operations ###
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

setwd('f:/ML/assignment-1-lokeshy')
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
bankdata = cbind(num_data,cat_data)
rm(num_attr,cat_attr,num_data,cat_data)

buildConfusionMatrix = function(actualData, prediction){
  conf_matrix = table(actualData,prediction)
  acc = sum(diag(conf_matrix))/ nrow(test)
  rec = (conf_matrix[2,2])/(conf_matrix[2,1]+conf_matrix[2,2])
  pre = conf_matrix[2,2]/sum(conf_matrix[,2])
  print(c("accuracy"=acc,"recall"=rec,"precision"=pre))
}



