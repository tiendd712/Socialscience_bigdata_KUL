library(caret)
library(dplyr)
library(randomForest)
library(xgboost)
library(DMwR2)

# Load data
gender_data = read_csv("final_processed_data/gender_processed.csv") %>% select(employee_id, gender_predict)
edu_data = read_csv("final_processed_data/edu_processed.csv")
lang_data = read_csv("final_processed_data/lang_processed.csv")
follower_data = read_csv("follower_data.csv")
connection_data = read_csv("connection_data.csv")
skill_data = read_csv("final_processed_data/skill_processed.csv")
exp_data = read_csv("data_model.csv")


# Preprocess the data
language_columns <- c("English", "French", "Dutch", "German", "Spanish", "Hindi", "Chinese")

df <- exp_data %>%
  merge(edu_data, by = "employee_id", all.x = TRUE) %>%
  filter(time_work > 0) %>% 
  merge(lang_data, by = "employee_id", all.x = TRUE) %>%
  merge(connection_data, by = "employee_id", all.x = TRUE) %>%
  merge(follower_data, by = "employee_id", all.x = TRUE) %>%
  merge(skill_data, by = "employee_id", all.x = TRUE) %>% 
  merge(gender_data, by = "employee_id", all.x = TRUE) %>% 
  select(-skills, -skill_trans, -strongest_skill, -skill4, -last_edu_year, -employee_id, -promote_level_1, -promote_general) %>%
  mutate(gender_predict = ifelse(is.na(gender_predict), "Neutral", gender_predict)) %>%
  mutate(promote_level_2 = ifelse(is.na(promote_level_2), 0, promote_level_2)) %>%
  mutate(highest_edu = ifelse(is.na(highest_edu), "bachelor", highest_edu)) %>%
  replace(is.na(.), 0) %>%
  mutate(promote_level_2 = factor(promote_level_2, levels = c(0, 1), labels = c(0, 1)),
         gender_predict = factor(gender_predict, levels = c("Neutral", "Man", "Woman"), labels = c(0,1,2)),
         highest_edu = factor(highest_edu, levels = c("bachelor", "master", "phd"), labels=c(0,1,2)),
         has_relevant_field = factor(has_relevant_field, levels = c(0, 1), labels = c(0, 1)),
         intern = factor(intern, levels = c(0, 1), labels = c(0, 1))) %>% 
  mutate_at(vars(all_of(language_columns)), factor,
            levels = c(0, 1))

dim(df)

glimpse(df)

sum(is.na(df)) > 0

# Split data into train and test sets
set.seed(7)
train_indices <- createDataPartition(y = df$promote_level_2, p = 0.8, list = FALSE)
data_train <- df[train_indices, ]
data_test <- df[-train_indices, ]
dim(data_train)
# Check probability of each type in train/test set
prop.table(table(data_train$promote_level_2))
prop.table(table(data_test$promote_level_2))

# Prepare data
x_train <- data_train %>% select(-promote_level_2)
y_train <- as.factor(data_train$promote_level_2)
x_test <- data_test %>% select(-promote_level_2)
y_test <- as.factor(data_test$promote_level_2)

# SMOTE
data_smote <- data_train
data_smote$promote_level_2 <- as.numeric(data_smote$promote_level_2)
data_smote$highest_edu <- as.numeric(data_smote$highest_edu)
data_smote$gender_predict <- as.numeric(data_smote$gender_predict)
data_smote <- SMOTE(data_smote, target=data_smote$promote_level_2, K=10, dup_size = 0)
data_smote$promote_level_2 <- as.factor(smote_train$promote_level_2)
data_smote$highest_edu <- as.factor(smote_train$highest_edu)
data_smote$gender_predict <- as.factor(smote_train$gender_predict)
gen_data_df <- as.data.frame(data_smote)


# Model
rf_model <- randomForest(x_train, y_train, ntree = 100)

rf_model <- randomForest(promote_level_2 ~ ., data = data_smote, ntree = 100)
print(rf_model)

# Predict
rf_predictions <- predict(rf_model, x_test)
sum(rf_predictions == 0)/length(rf_predictions) * 100
sum(rf_predictions == 1)/length(rf_predictions) * 100
shap_values=predict(rf_model, x_test, predcontrib = TRUE, approxcontrib = F)

# Evaluate
conf_matrix <- confusionMatrix(rf_predictions, y_test, positive='1')
conf_matrix
conf_matrix$byClass

# Try changing the threshhold
model <- glm(promote_level_2 ~ .,data=data_train, family=binomial(link = "logit"))
model_summary <- summary(model)
print(model_summary)
pred <- predict(model, data_test, type="response")
c_matrix <- confusionMatrix(data= as.factor(as.numeric(pred>0.5)), reference = data_test$promote_level_2, positive = "1")
c_matrix
c_matrix$byClass
 

