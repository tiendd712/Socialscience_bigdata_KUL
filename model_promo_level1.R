library(caret)
library(tidyverse)
library(randomForest)
library(DMwR2)
library(fastDummies)
library(shapper)



## ================= Load and process data =============================================


gender_data = read_csv("https://raw.githubusercontent.com/tiendd712/Socialscience_bigdata_KUL/master/data_processing/final_processed_data/gender_processed.csv") %>% select(employee_id, gender_predict)
edu_data = read_csv("https://raw.githubusercontent.com/tiendd712/Socialscience_bigdata_KUL/master/data_processing/final_processed_data/edu_processed.csv")
lang_data = read_csv("https://raw.githubusercontent.com/tiendd712/Socialscience_bigdata_KUL/master/data_processing/final_processed_data/lang_processed.csv")
follower_data = read_csv("https://raw.githubusercontent.com/tiendd712/Socialscience_bigdata_KUL/master/data_processing/follower_data.csv")
connection_data = read_csv("https://raw.githubusercontent.com/tiendd712/Socialscience_bigdata_KUL/master/data_processing/connection_data.csv")
skill_data = read_csv("https://raw.githubusercontent.com/tiendd712/Socialscience_bigdata_KUL/master/data_processing/final_processed_data/skill_processed.csv")
exp_data = read_csv("https://raw.githubusercontent.com/tiendd712/Socialscience_bigdata_KUL/master/data_processing/final_processed_data/exp_processed.csv")

language_columns <- c("English", "French", "Dutch", "German", "Spanish", "Hindi", "Chinese")


# Preprocess the data
df_level_1 = exp_data %>%
  merge(edu_data, by = "employee_id", all.x = TRUE) %>%
  filter(time_work > 0) %>% 
  merge(lang_data, by = "employee_id", all.x = TRUE) %>%
  merge(connection_data, by = "employee_id", all.x = TRUE) %>%
  merge(follower_data, by = "employee_id", all.x = TRUE) %>%
  merge(skill_data, by = "employee_id", all.x = TRUE) %>% 
  merge(gender_data, by = "employee_id", all.x = TRUE) %>% 
  filter(!(promote_level_1 ==  0 & promote_level_2 == 1)) %>% 
  select(-skills, -skill_trans, -strongest_role, -skill4, -last_edu_year, -promote_general, -promote_level_2, -connection_count, -employee_id) %>%
  mutate(gender_predict = ifelse(is.na(gender_predict), "Neutral", gender_predict)) %>%
  mutate(promote_level_1 = ifelse(is.na(promote_level_1), 0, promote_level_1)) %>%
  mutate(highest_edu = ifelse(is.na(highest_edu), "bachelor", highest_edu)) %>%
  replace(is.na(.), 0) %>%
  mutate(promote_level_1 = factor(promote_level_1, levels = c(0, 1), labels = c(0, 1)),
         gender_predict = factor(gender_predict, levels = c("Man", "Woman", "Neutral"), labels = c(0, 1, 2)),
         highest_edu = factor(highest_edu, levels = c("bachelor", "master", "phd"), labels = c(0, 1, 2)),
         has_relevant_field = factor(has_relevant_field, levels = c(0, 1), labels = c(0, 1)),
         intern = factor(intern, levels = c(0, 1), labels = c(0, 1)),
         connection_type = factor(connection_type, levels = c(0, 1), labels = c(0, 1))) %>% 
  mutate_at(vars(all_of(language_columns)), factor,
            levels = c(0, 1))




# Split data into train and test sets
set.seed(7)
train_indices <- createDataPartition(y = df_level_1$promote_level_1, p = 0.8, list = FALSE)
data_train_level_1 <- df_level_1[train_indices, ]
data_test_level_1 <- df_level_1[-train_indices, ]
dim(data_train_level_1)
dim(data_test_level_1)

# Check probability of each type in train/test set
prop.table(table(data_train_level_1$promote_level_1))
prop.table(table(data_test_level_1$promote_level_1))

# Prepare data
x_train_level_1 <- data_train_level_1 %>% select(-promote_level_1)
y_train_level_1 <- as.factor(data_train_level_1$promote_level_1)
x_test_level_1 <- data_test_level_1 %>% select(-promote_level_1)
y_test_level_1 <- as.factor(data_test_level_1$promote_level_1)
print(levels(y_train_level_1))
print(levels(y_test_level_1))


write.csv(df_level_1, "C:/Users/doduc/df_level_1.csv", row.names = F)

write.csv(train_indices, "C:/Users/doduc/train_indices.csv", row.names = F)
# ================================ Logistics model ==============================

model_level_1 <- glm(promote_level_1 ~ .,data=data_train_level_1, family=binomial(link = "logit"))
summary(model_level_1)

# =============================== XGboost model ===========================

## One hot encoding gender and highest_edu  

df_level_1_e = dummy_cols(df_level_1, select_columns = c("highest_edu", "gender_predict"),
                          remove_selected_columns = T) 



data_train_level_1_e <- df_level_1_e[train_indices, ]
data_test_level_1_e <- df_level_1_e[-train_indices, ]



x_train_level_1_e <- data_train_level_1_e %>% select(-promote_level_1)

x_test_level_1_e <- data_test_level_1_e %>% select(-promote_level_1)



## XGboost model need numeric input


for (i in c(1:ncol(x_train_level_1_e))){
  if (is.factor(x_train_level_1_e[,i])){
    x_train_level_1_e[,i] = as.numeric(x_train_level_1_e[,i]) - 1
  }
}

for (i in c(1:ncol(x_test_level_1_e))){
  if (is.factor(x_test_level_1_e[,i])){
    x_test_level_1_e[,i] = as.numeric(x_test_level_1_e[,i]) - 1
  }
}



## Tunning hyperparameter for XGboost

# Define the hyperparameter grid

set.seed(7)
param_grid <- data.frame(
  nrounds = sample(50:200, 50),                   
  max_depth = sample(1:50, 50, replace = T),                    
  eta = runif(50, 0.01, 0.5),                      
  colsample_bytree = runif(50, 0.6, 1),
  min_child_weight = sample(1:10, 50, replace = T),
  gamma = runif(50, 0, 1),
  subsample = runif(50, 0.7, 1)
)


# Specify the cross-validation settings
ctrl <- trainControl(
  method = "cv",              
  number = 5,                 
  search = "random",          
  verboseIter = TRUE          
)



# Perform Randomized Search Cross-Validation using the xgb.train function
tunning_model <- train(
  x = as.matrix(x_train_level_1_e),
  y = y_train_level_1,
  method = "xgbTree",          # Specifies the xgboost model
  trControl = ctrl,            # Use the control settings from step 4
  tuneGrid = param_grid        # Use the hyperparameter grid from step 3
)

write.csv(tunning_model$bestTune,
          "C:/Users/doduc/Github/Socialscience_bigdata_KUL/tunning_model_level1.csv",
          row.names = F)


## model

params <- list(
  objective = "binary:logistic",   
  eval_metric = "error",         
  nrounds = tunning_model$bestTune$nrounds,                   
  max_depth = tunning_model$bestTune$max_depth,
  eta = tunning_model$bestTune$eta,
  gamma = tunning_model$bestTune$gamma,
  colsample_bytree = tunning_model$bestTune$colsample_bytree,
  min_child_weight = tunning_model$bestTune$min_child_weight,
  subsample = tunning_model$bestTune$subsample
  
)


# Train the XGBoost model
xgb_model <- xgboost(data = as.matrix(x_train_level_1_e), 
                      label =  as.numeric(y_train_level_1) - 1, 
                      params = params,
                      nrounds = tunning_model$bestTune$nrounds)


# Make prediction
predictions = predict(model, as.matrix(x_test_level_1_e))


predictions <- ifelse(predictions >= 0.5, 1, 0)



confusionMatrix(factor(predictions), y_test_level_1, positive="1")
                                                                                                        


# SHAP plot
p = xgboost::xgb.plot.shap.summary(data = as.matrix(x_train_level_1_e),
                                   model = xgb_model)


p + ggplot2::scale_colour_viridis_c(limits = c(-2, 4),
                                    option = "plasma", direction = -1) +
  labs(y = "SHAP value", x = "Feature") + 
  theme_bw()
  
