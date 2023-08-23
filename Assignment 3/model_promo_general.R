library(caret)
library(dplyr)
library(randomForest)
library(xgboost)
library(ROSE)
library(readr)

# Load data
gender_data <- read_csv("gender_processed.csv") %>% select(employee_id, gender_predict)
edu_data <- read_csv("edu_processed.csv")
lang_data <- read_csv("lang_processed.csv")
follower_data <- read_csv("follower_data.csv")
connection_data <- read_csv("connection_data.csv")
skill_data <- read_csv("skill_processed.csv")
exp_data <- read_csv("data_model.csv")

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
  select(-skills, -skill_trans, -strongest_skill, -skill4, -last_edu_year, -employee_id, -promote_level_1, -promote_level_2) %>%
  mutate(gender_predict = ifelse(is.na(gender_predict), "Neutral", gender_predict)) %>%
  mutate(promote_general = ifelse(is.na(promote_general), 0, promote_general)) %>%
  mutate(highest_edu = ifelse(is.na(highest_edu), "bachelor", highest_edu)) %>%
  replace(is.na(.), 0) %>%
  mutate(promote_general = factor(promote_general, levels = c(0, 1), labels = c(0, 1)),
         gender_predict = factor(gender_predict, levels = c("Neutral", "Man", "Woman"), labels = c(0, 1, 2)),
         highest_edu = factor(highest_edu, levels = c("bachelor", "master", "phd"), labels = c(0, 1, 2)),
         has_relevant_field = factor(has_relevant_field, levels = c(0, 1), labels = c(0, 1)),
         intern = factor(intern, levels = c(0, 1), labels = c(0, 1))) %>% 
  mutate_at(vars(all_of(language_columns)), factor,
            levels = c(0, 1))

dim(df)

glimpse(df)

sum(is.na(df)) > 0

# Split data into train and test sets
set.seed(7)
train_indices <- createDataPartition(y = df$promote_general, p = 0.8, list = FALSE)
data_train <- df[train_indices, ]
data_test <- df[-train_indices, ]
dim(data_train)

# Check probability of each type in train/test set
prop.table(table(data_train$promote_general))
prop.table(table(data_test$promote_general))

# Prepare data
x_train <- data_train %>% select(-promote_general)
y_train <- as.factor(data_train$promote_general)
x_test <- data_test %>% select(-promote_general)
y_test <- as.factor(data_test$promote_general)
print(levels(y_train))
print(levels(y_test))

# SMOTE with ROSE
data_smote <- data_train
data_smote$promote_general <- as.factor(data_smote$promote_general)
smote_train <- ovun.sample(promote_general ~ ., data = data_smote, method = "both", N = length(data_smote), seed = 1234)$data

# Model - Random Forest
rf_model <- randomForest(x_train, y_train, ntree = 100, classwt = c(1, 1))
print(rf_model)

# Predict - Random Forest
rf_predictions <- predict(rf_model, x_test)
sum(rf_predictions == 0) / length(rf_predictions) * 100
sum(rf_predictions == 1) / length(rf_predictions) * 100

# Evaluate - Random Forest
conf_matrix_rf <- confusionMatrix(rf_predictions, y_test, positive = '1')
conf_matrix_rf
conf_matrix_rf$byClass

# Try changing the threshhold
model <- glm(promote_general ~ .,data=data_train, family=binomial(link = "logit"))
model_summary <- summary(model)
print(model_summary)
pred <- predict(model, data_test, type="response")
c_matrix <- confusionMatrix(data= as.factor(as.numeric(pred>0.3)), reference = data_test$promote_general, positive = "1")
c_matrix

