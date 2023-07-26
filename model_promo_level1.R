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


# ================================ Logistics model ==============================

model_level_1 <- glm(promote_level_1 ~ .,data=data_train_level_1, family=binomial(link = "logit"))
summary(model_level_1)

# =============================== Random forest model ===========================

## One hot encoding gender and highest_edu  

df_level_1_e = dummy_cols(df_level_1, select_columns = c("highest_edu", "gender_predict"),
                          remove_selected_columns = T) 



for (i in c(24:29)){
  df_level_1_e[,i] = as.factor(df_level_1_e[,i])
  }

data_train_level_1_e <- df_level_1_e[train_indices, ]
data_test_level_1_e <- df_level_1_e[-train_indices, ]



x_train_level_1_e <- data_train_level_1_e %>% select(-promote_level_1)

x_test_level_1_e <- data_test_level_1_e %>% select(-promote_level_1)

## Tunning hyperparameter for random forest

# Define the hyperparameter grid
param_grid <- expand.grid(
  ntree = c(50, 100, 150),
  mtry = c(4, 6, 8),
  max_depth = c(10, 20, 30)
)

# Initialize variables to store best model and performance
best_model <- NULL
best_f1_score <- 0

# Perform grid search
for (i in 1:nrow(param_grid)) {
  rf_model <- randomForest(
    x_train_level_1_e, y_train_level_1,
    ntree = param_grid$ntree[i],
    mtry = param_grid$mtry[i],
    max_depth = param_grid$max_depth[i]
  )
  
  rf_predictions <- predict(rf_model, x_test_level_1_e)
  conf_matrix <- confusionMatrix(rf_predictions, y_test_level_1)
  f1_score <- conf_matrix$byClass['F1'][1]
  
  if (f1_score > best_f1_score) {
    best_model <- rf_model
    best_f1_score <- f1_score
  }
}

# Evaluate the best model
best_rf_predictions <- predict(rf_model, x_test_level_1_e)
conf_matrix <- confusionMatrix(best_rf_predictions, y_test_level_1, positive='1')




rf_model <- randomForest(
  x_train_level_1_e, y_train_level_1,
  ntree = 85,
  max_depth = 7,
  nodesize = 1,
  
)








s <- kernelshap(rf_model, x_train_level_1_e, bg_X = x_train_level_1_e[c(1:500),])

# Step 2: Turn them into a shapviz object
sv <- shapviz(s)


shapper::
































































 



























