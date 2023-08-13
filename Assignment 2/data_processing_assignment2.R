library(tidyverse)
library(readxl)
library(caret)


setwd("C:/Users/doduc/OneDrive - KU Leuven/My knowledge/Master of Statistics_KUL/Collecting big data/data")


ess_data = read_csv("ESS-Data-Wizard-subset-2023-08-03 final list/ESS-Data-Wizard-subset-2023-08-03.csv")


var_pick = read_excel("C:/Users/doduc/Github/Socialscience_bigdata_KUL/Assignment 2/var_pick.xlsx",
                      sheet = 1) 


## remove columns are not chosen 

ess_data = ess_data[, str_trim(var_pick$var)]

var_pick$var = str_trim(var_pick$var)

var_pick$missing_value = str_trim(var_pick$missing_value)

## Assign NA value for columns

for (stt in c(2:ncol(ess_data))){
  
  command_text = str_c("ess_data = ess_data %>% mutate(", 
                       var_pick[stt, "var", drop = T],
                       "= case_when(",
                       var_pick[stt, "var", drop = T],
                       " %in% ",
                       var_pick[stt, "missing_value", drop = T],
                       " ~ NA, TRUE ~ ", var_pick[stt, "var", drop = T],
                       "))")
  
  
  eval(parse(text = command_text))
  
  
}


## Check NA value 

check_na_ratio = data.frame()
for (i in c(1:ncol(ess_data))){
  check_na_ratio = rbind(check_na_ratio,
                         data.frame(col_name = names(ess_data)[i],
                                    na_ratio = sum(is.na(ess_data[,i, drop = T]))*100/nrow(ess_data)))
}


## drop na in target variable

ess_data  = ess_data %>% filter(!is.na(imwbcnt))


for (col in var_pick %>% filter(data_type == "nominal") %>% .$var){
  
  ess_data[, col] = as.factor(ess_data[, col, drop = T])
}


## remove country, pdjobev, marsts variable

ess_data = ess_data %>% select(-cntry, -pdjobev, -marsts)


## one hot encoding nominal variable

dummy = dummyVars("~ .",data = ess_data )


ess_data_e = data.frame(predict(dummy, newdata=ess_data ))


## building model to predict target variable

## split train, test data


set.seed(7)
train_indices <- createDataPartition(y = ess_data_e$imwbcnt, p = 0.8, list = FALSE)
data_train <- ess_data_e[train_indices, ]
data_test <- ess_data_e[-train_indices, ]
dim(data_train)
dim(data_test)

## create input data and target variable 

x_train = data_train %>% select(-imwbcnt)
y_train = as.factor(data_train$imwbcnt)

x_test = data_test %>% select(-imwbcnt)
y_test = as.factor(data_test$imwbcnt)

## 


rf_model <- train(imwbcnt ~ ., data = data_train, method = "rf")



write.csv(data_train, "data_train.csv", row.names = F)
write.csv(data_test, "data_test.csv", row.names = F)


