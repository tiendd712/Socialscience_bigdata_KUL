library(caret)
library(tidyverse)
library(randomForest)
library(xgboost)
library(DMwR2)

setwd("C:/Users/doduc/Github/Socialscience_bigdata_KUL/data_processing/")
getwd()

# Load data
gender_data = read_csv("final_processed_data/gender_processed.csv") %>% select(employee_id, gender_predict)
edu_data = read_csv("final_processed_data/edu_processed.csv")
lang_data = read_csv("final_processed_data/lang_processed.csv")
follower_data = read_csv("follower_data.csv")
connection_data = read_csv("connection_data.csv")
skill_data = read_csv("final_processed_data/skill_processed.csv")
exp_data = read_csv("data_model.csv")


# Preprocess the data
df_level_1 = exp_data %>%
  merge(edu_data, by = "employee_id", all.x = TRUE) %>%
  filter(time_work > 0) %>% 
  merge(lang_data, by = "employee_id", all.x = TRUE) %>%
  merge(connection_data, by = "employee_id", all.x = TRUE) %>%
  merge(follower_data, by = "employee_id", all.x = TRUE) %>%
  merge(skill_data, by = "employee_id", all.x = TRUE) %>% 
  merge(gender_data, by = "employee_id", all.x = TRUE) %>% 
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
train_indices <- createDataPartition(y = df$promote_level_1, p = 0.8, list = FALSE)
data_train <- df[train_indices, ]
data_test <- df[-train_indices, ]
dim(data_train)
# Check probability of each type in train/test set
prop.table(table(data_train$promote_level_1))
prop.table(table(data_test$promote_level_1))

# Prepare data
x_train <- data_train %>% select(-promote_level_1, -promote_level_2, -promote_general, -employee_id)
y_train <- as.factor(data_train$promote_level_1)
x_test <- data_test %>% select(-promote_level_1, -promote_level_2, -promote_general, -employee_id)
y_test <- as.factor(data_test$promote_level_1)

print(levels(y_train))
print(levels(y_test))

# SMOTE
# data_smote <- data_train
# data_smote$promote_level_2 <- as.numeric(data_smote$promote_level_2)
# data_smote$highest_edu <- as.numeric(data_smote$highest_edu)
# data_smote$gender_predict <- as.numeric(data_smote$gender_predict)
# smote_train <- SMOTE(data_train, target=data_train$promote_level_2, K=5, dup_size = 0)
# smote_train$promote_level_2 <- as.factor(smote_train$promote_level_2)
# smote_train$highest_edu <- as.factor(smote_train$highest_edu)
# smote_train$gender_predict <- as.factor(smote_train$gender_predict)

# Model
rf_model <- randomForest(x_train, y_train, ntree = 100)
# rf_model_smote <- randomForest(smote_train, y_train, ntree = 100)
print(rf_model)

# Predict
rf_predictions <- predict(rf_model, x_test)
sum(rf_predictions == 0)/length(rf_predictions) * 100
sum(rf_predictions == 1)/length(rf_predictions) * 100

# Evaluate
conf_matrix <- confusionMatrix(rf_predictions, y_test, positive='1')
conf_matrix
conf_matrix$byClass






# Try changing the threshhold
model <- glm(promote_level_2 ~ .,data=data_train, family=binomial(link = "logit"))
model_summary <- summary(model)
print(model_summary)
pred <- predict(model, data_test, type="response")
c_matrix <- confusionMatrix(data= as.factor(as.numeric(pred>0.3)), reference = data_test$promote_level_2, positive = "1")
c_matrix





## building xgboost model

## cross validation
# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "grid")

set.seed(7)
# Customsing the tuning grid
gbmGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                        nrounds = (1:10)*50,    # number of trees
                        # default values below
                        eta = 0.3,
                        gamma = 0,
                        subsample = 1,
                        min_child_weight = 1,
                        colsample_bytree = 0.6)

# training a XGboost Regression tree model while tuning parameters
model = train(y_train~., data = data_train, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid)

# summarising the results
print(model)


write.csv(df, "df.csv")


data_train = data_train %>% select(-promote_level_2, -promote_general, -employee_id)


model <- glm(promote_level_1 ~ .,data=data_train, family=binomial(link = "logit"))
model_summary <- summary(model)
print(model_summary)


## =====================================================================================


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




# Load required libraries
library(randomForest)
library(gbm)
library(xgboost)





# Define the model parameters
model_params <- list(
  random_forest = list(
    model = "randomForest",
    params = list(
      ntree = sample(50:100, 1),
      mtry = sample(3:50, 1),
      nodesize = sample(2:20, 1),
      classwt = c(1, 1),
      importance = TRUE,
      do.trace = FALSE,
      keep.forest = FALSE,
      replace = TRUE
    )
  ),
  gradient_boosting = list(
    model = "gbm",
    params = list(
      n.trees = sample(50:100, 1),
      shrinkage = runif(1, 0.01, 0.5),
      interaction.depth = sample(1:10, 1),
      n.minobsinnode = sample(2:20, 1),
      bag.fraction = 0.5,
      train.fraction = 1.0,
      verbose = FALSE
    )
  ),
  xgboost = list(
    model = "xgboost",
    params = list(
      nrounds = sample(50:100, 1),
      eta = runif(1, 0.01, 0.5),
      max_depth = sample(1:10, 1),
      min_child_weight = sample(1:10, 1),
      gamma = runif(1, 0, 1),
      alpha = runif(1, 0, 1),
      lambda = runif(1, 0, 1)
    )
  )
)

params_dict <- list()

# Loop through each model in model_params and run RandomizedSearchCV
for (model_name in names(model_params)) {
  cat(paste("Running RandomizedSearchCV for ", model_name, "...\n"))
  
  # Create a RandomizedSearchCV object for the current model
  model_info <- model_params[[model_name]]
  model <- model_info$model
  
  if (model == "randomForest") {
    model_fn <- randomForest
  } else if (model == "gbm") {
    model_fn <- gbm
  } else if (model == "xgboost") {
    model_fn <- xgboost
  } else {
    stop(paste("Model '", model, "' not recognized."))
  }
  
  param_dist <- model_info$params
  set.seed(7)  # Set random seed for reproducibility
  

  # Call the model function with the correct arguments
  random_search <- do.call(model_fn, c(list(x = x_train_level_1, y = y_train_level_1), param_dist))
  
  # Print the best parameters and score
  params_dict[[model_name]] <- random_search$bestTune
  cat(paste("Best parameters for ", model_name, ": ", as.character(random_search$bestTune), "\n"))
  cat(paste("Best score for ", model_name, ": ", max(random_search$results$accuracy), "\n"))
  cat("\n")
}
 

rf_model_level_1 <- randomForest(x_train_level_1, y_train_level_1, ntree = 70, classwt = c(1, 1),
                                 ntry = 16, nodesize = 6, importance = TRUE,
                                 do.trace = FALSE,
                                 keep.forest = FALSE,
                                 replace = TRUE
                                 )
print(rf_model_level_1)