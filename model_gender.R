library(caret)
library(dplyr)
library(randomForest)
library(xgboost)

# Load data
gender_data = read_csv("final_processed_data/gender_processed.csv") %>% select(employee_id, gender_predict)
edu_data = read_csv("final_processed_data/edu_processed.csv")
lang_data = read_csv("final_processed_data/lang_processed.csv")
follower_data = read_csv("follower_data.csv")
connection_data = read_csv("connection_data.csv")
skill_data = read_csv("final_processed_data/skill_processed.csv")


# Preprocess the data
df <- gender_data %>%
  mutate(gender_predict = ifelse(is.na(gender_data$gender_predict), "Neutral", gender_data$gender_predict)) %>% 
  merge(edu_data, by = "employee_id", all.x = TRUE) %>%
  merge(lang_data, by = "employee_id", all.x = TRUE) %>%
  merge(connection_data, by = "employee_id", all.x = TRUE) %>%
  merge(follower_data, by = "employee_id", all.x = TRUE) %>%
  merge(skill_data, by = "employee_id", all.x = TRUE) %>% 
  filter(gender_predict != "Neutral") %>% 
  mutate(highest_edu = factor(highest_edu, levels = c(1, 2, 3), labels = c('bachelor', 'master', 'phd'))) %>% 
  mutate(highest_edu = ifelse(is.na(highest_edu), 1, highest_edu)) %>%
  mutate(gender_label = factor(gender_predict, levels = c("Woman", "Man"), labels = c(1, 2))) %>% 
  select(-skills, -skill_trans, -strongest_skill, -skill4, -last_edu_year, -employee_id, -gender_predict) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))

glimpse(df)

# Split data into train and test sets
set.seed(7)
train_indices <- createDataPartition(y = df$gender_label, p = 0.8, list = FALSE)
data_train <- df[train_indices, ]
data_test <- df[-train_indices, ]
dim(data_train)
# Check probability of each type in train/test set
prop.table(table(data_train$gender_label))
prop.table(table(data_test$gender_label))

# Prepare data
x <- data_train %>% select(-gender_label)
y <- as.factor(data_train$gender_label)

# Model
rf_model <- randomForest(x, y, ntree = 100)
print(rf_model)

# Predict
rf_predictions <- predict(rf_model, x)
percent_woman <- sum(rf_predictions == 1)/length(rf_predictions) * 100
percent_man <- sum(rf_predictions == 2)/length(rf_predictions) * 100

# Evaluate
# Extracting evaluation metrics for woman (label 1)
conf_matrix <- confusionMatrix(rf_predictions, y)
conf_matrix 
f1_score_woman <- conf_matrix$byClass['F1'][1]
recall_woman <- conf_matrix$byClass['Sensitivity'][1]
precision_woman <- conf_matrix$byClass['Pos Pred Value'][1]

# Print evaluation metrics for woman
cat("F1 Score for Woman:", f1_score_woman, "\n")
cat("Recall for Woman:", recall_woman, "\n")
cat("Precision for Woman:", precision_woman, "\n")

