library(tidyverse)
library(readxl)
library(caret)
library(randomForest)
library(kernelshap)
library(shapviz)


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



## remove country, pdjobev, marsts variable

ess_data = ess_data %>% select(-cntry, -pdjobev, -marsts)



## drop na in target variable

ess_data  = ess_data %>% filter(!is.na(imwbcnt))


for (col in var_pick %>% filter(data_type == "nominal") %>% .$var){
  if(col %in% colnames(ess_data)) {
    
    ess_data[, col] = as.factor(ess_data[, col, drop = T])
  }
}



## one hot encoding nominal variable

dummy = dummyVars("~ .",data = ess_data )


ess_data_e = data.frame(predict(dummy, newdata=ess_data ))


## remove imwbcnt with value of 4, 5, 6 and encode 0 -> 3: disagree, 7-10: agree

ess_data_e = ess_data_e %>% filter(!(imwbcnt %in% c(4, 5, 6)))


ess_data_e = ess_data_e %>% 
  mutate(imwbcnt = case_when(between(imwbcnt, 0, 3) ~ 0,
                                   T ~ 1))



## building model to predict target variable

## split train, test data


set.seed(7)
train_indices <- createDataPartition(y = ess_data_e$imwbcnt, p = 0.8, list = FALSE)
data_train <- ess_data_e[train_indices, ]
data_test <- ess_data_e[-train_indices, ]
dim(data_train)
dim(data_test)


## drop na for every column

data_train_drop_all = data_train %>% drop_na()


## random forest to pick up 15 importance features

model_test <- randomForest(as.factor(imwbcnt) ~ ., data = data_train_drop_all)

importance_df = data.frame(
  feature = rownames(importance(model_test)),
  importance = importance(model_test)
)

names(importance_df) = c("feature", 'importance')




ggplot(data = importance_df %>% arrange(desc(importance)) %>% head(15), 
       aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Feature", y = "Importance") 



importance_feature = importance_df %>% 
  arrange(desc(importance)) %>% 
  head(15) %>% .$feature 



## create input data and target variable 

data_train = data_train[c(importance_feature, "imwbcnt")] %>% drop_na()

data_test= data_test[c(importance_feature, "imwbcnt")] %>% drop_na()


x_train = data_train %>% select(-imwbcnt)
y_train = data_train$imwbcnt

x_test = data_test %>% select(-imwbcnt)
y_test = data_test$imwbcnt


### Model building

## randomforest

train_control = trainControl(method='cv', number = 5)


rf_model = train(x = x_train,y = as.factor(y_train), method = 'rf', 
                 trControl = train_control)



rf_pred = predict(rf_model, newdata = x_test)


print(confusionMatrix(rf_pred, as.factor(y_test), mode = "everything",positive = '1'))



saveRDS(rf_model, "C:/Users/doduc/Github/Socialscience_bigdata_KUL/Assignment 2/rf_model.rds")


## gradient boosting


gb_model = train(x = x_train,y = as.factor(y_train), method = 'gbm', 
                 trControl = train_control)

gb_pred = predict(gb_model, newdata = x_test)


print(confusionMatrix(gb_pred, as.factor(y_test), mode = "everything",positive = '1'))

saveRDS(gb_model, "C:/Users/doduc/Github/Socialscience_bigdata_KUL/Assignment 2/gb_model.rds")



## xgboost

xg_model = train(x = x_train,y = as.factor(y_train), method = 'xgbTree', 
                 trControl = train_control)

xg_pred = predict(xg_model, newdata = x_test)


print(confusionMatrix(xg_pred, as.factor(y_test), mode = "everything",positive = '1'))


saveRDS(xg_model, "C:/Users/doduc/Github/Socialscience_bigdata_KUL/Assignment 2/xg_model.rds")



## Logistics regression

lr_model = train(x = x_train,y = as.factor(y_train), method = 'glm', family='binomial', 
                 trControl = train_control)

lr_pred = predict(lr_model, newdata = x_test)



print(confusionMatrix(lr_pred, as.factor(y_test), mode = "everything",positive = '1'))


saveRDS(lr_model, "C:/Users/doduc/Github/Socialscience_bigdata_KUL/Assignment 2/lr_model.rds")


summary(lr_model)

## SVM
svm_model <- train(x = x_train, y = as.factor(y_train), method = 'svmLinear3', trControl = train_control)
svm_pred <- predict(svm_model, newdata=x_test)
print(confusionMatrix(svm_pred, as.factor(y_test),mode = "everything",positive = '1'))

## SHAP PLOT


my_pred_fun <- function(model, data) {
  predictions <- as.numeric(predict(model, newdata = data)) - 1
  return(predictions)
}

s = kernelshap(rf_model, X = x_train, bg_X = x_train[1:10,],
               pred_fun  = my_pred_fun)


# Step 2: Turn them into a shapviz object
sv <- shapviz(s)


saveRDS(sv, "C:/Users/doduc/Github/Socialscience_bigdata_KUL/Assignment 2/sv.rds")


xgboost()

sv_importance(sv)


cv = xgboost::xgb.cv(nfold = 5,
                     data = as.matrix(x_train),
                     label = as.factor(y_train))

xgboost::xgb.plot.shap.summary()



explainer <- iml::Shapley(rf_model, data = x_train)
explainer <- shapr(x_train, xg_model)

View(shapr:::get_supported_models())



iml::Shapley()


rf_model <- NULL  


if(!file.exists("C:/Users/doduc/Github/Socialscience_bigdata_KUL/Assignment 2/rf_model.rds")){
  rf_model = train(x = x_train,y = as.factor(y_train), method = 'rf', 
                   trControl = train_control)
} else {
  rf_model = readRDS("C:/Users/doduc/Github/Socialscience_bigdata_KUL/Assignment 2/rf_model.rds")
  
}