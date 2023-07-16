library(tidyverse)
library(jsonlite)
library(tokenizers)
library(tidytext)
library(quanteda)
library(lubridate)

### load data
linkedin_profile =  read_json("C:/Users/doduc/OneDrive - KU Leuven/My knowledge/Master of Statistics_KUL/Collecting big data/SS_bigdata/all_profiles.json")



unique_id = c()
for (i in c(1:10977)){
  unique_id = c(unique_id, str_c("t_", i))
}
  

names(linkedin_profile) = unique_id


### =========================== create connection data ============================= 

connection_extract = function(employee_id){
  list_connect = linkedin_profile[[employee_id]]$network
  if(is.null(list_connect$connectionsCount)){connection_count = NA}
  else{connection_count = list_connect$connectionsCount}
  return(connection_count)
}

connection_count = c()
employee_vec = c()


for (employee_id in names(linkedin_profile)){
  connection_count = c(connection_count, connection_extract(employee_id))
  employee_vec = c(employee_vec, employee_id)
}

connection_data = data.frame(employee_id = employee_vec,
                             connection_count = connection_count)

connection_data %>% filter(connection_count < 20)

write.csv(connection_data, "connection_data.csv", row.names = FALSE)



### ========================= create experience data ===========================

#### create function extract data experience for each person


experience_extract = function(employee_id){
  list_ex = linkedin_profile[[employee_id]]$experience
  num_ex = length(list_ex)
  title = c()
  companyname = c()
  locationname = c()
  description = c()
  industry = c()
  start_date = c()
  end_date = c()
  companyurn = c()
  employee_range_start = c()
  employee_range_end = c()
  
  if (num_ex==0){return(data.frame())}

    
  for (ex in c(1:num_ex)){
    title_value = unlist(list_ex[[ex]]["title"])
    if(is.null(title_value)){title_value = NA}
    title = c(title, title_value)
    
    company_value = unlist(list_ex[[ex]]["companyName"])
    if(is.null(company_value)){company_value = NA}
    companyname = c(companyname, company_value)
    
    location_value = unlist(list_ex[[ex]]["locationName"])
    if(is.null(location_value)){location_value = NA}
    locationname = c(locationname, location_value)
    
    description_value = unlist(list_ex[[ex]]["description"])
    if(is.null(description_value)){description_value = NA}
    description = c(description, description_value)
    
    industry_value = unlist(list_ex[[ex]]["company"])["company.industries"]
    if(is.null(industry_value)){industry_value = NA}
    industry = c(industry, industry_value)
    
    companyurn_value = unlist(list_ex[[ex]]["companyUrn"])
    if(is.null(companyurn_value)){companyurn_value = NA}
    companyurn = c(companyurn, companyurn_value)
    
    
    employee_range_start_value =  unlist(list_ex[[ex]]["company"])["company.employeeCountRange.start"]
    if(is.null(employee_range_start_value)){employee_range_start_value = NA}
    employee_range_start = c(employee_range_start, employee_range_start_value)
    
    
    employee_range_end_value =  unlist(list_ex[[ex]]["company"])["company.employeeCountRange.end"]
    if(is.null(employee_range_end_value)){employee_range_end_value = NA}
    employee_range_end = c(employee_range_end, employee_range_end_value)
    
    start_value = as.Date(paste("01", 
                                unlist(list_ex[[ex]]["timePeriod"])["timePeriod.startDate.month"], 
                                unlist(list_ex[[ex]]["timePeriod"])["timePeriod.startDate.year"],
                                sep = "-"), 
                          format = "%d-%m-%Y")
    start_date = c(start_date, start_value)
    end_value = as.Date(paste("01", 
                              unlist(list_ex[[ex]]["timePeriod"])["timePeriod.endDate.month"], 
                              unlist(list_ex[[ex]]["timePeriod"])["timePeriod.endDate.year"],
                              sep = "-"), 
                        format = "%d-%m-%Y")
    end_date = c(end_date, end_value)
    
  }
  experience_data = data.frame(employee_id = rep(employee_id, length(list_ex)),
                               title = unname(title),
                               companyname = unname(companyname),
                               locationname = unname(locationname),
                               description = unname(description),
                               industry = unname(industry),
                               companyurn = unname(companyurn),
                               employee_range_start = as.numeric(unname(employee_range_start)),
                               employee_range_end = as.numeric(unname(employee_range_end)),
                               start_date = as.Date(unname(start_date), origin = "1970-01-01"),
                               end_date = as.Date(unname(end_date), origin = "1970-01-01"))
  
  return(experience_data)
}

experience_data = experience_extract("t_1")

for (employee_id in names(linkedin_profile)[-1]){
  experience_data = rbind(experience_data,
                          experience_extract(employee_id))
}


## assign category for company
experience_data = experience_data %>% 
  mutate(employer_cate = case_when(employee_range_end < 100 ~  0,
                                   employee_range_end <= 999 & employee_range_start >= 100 ~  1,
                                   employee_range_end <= 9999 & employee_range_start >= 1000 ~  2,
                                   employee_range_start >= 10000 ~  3))


experience_data$companyurn = as.numeric(unlist(str_extract_all(experience_data$companyurn, "\\d+")))



## exclude the profile having less than 20 connection (suspect clone profile)
experience_data = experience_data %>% filter(!(employee_id %in% (connection_data %>% filter(connection_count < 20) %>% .$employee_id)))




write.csv(experience_data, "experience_data.csv", row.names = FALSE)


## experience_data = read.csv(str_c(link, "experience_data.csv"))




### ============================= create education data ===============================



education_extract = function(employee_id){
  
  schoolName = c()
  fieldOfStudy = c()
  degreeName = c()
  
  for (i in c(1:length(linkedin_profile[[employee_id]]$education))){
    
    if(is.null(linkedin_profile[[employee_id]]$education[[i]]$schoolName)){
      
      schoolName = c(schoolName, NA)
      
    }
    
    else{schoolName = c(schoolName, linkedin_profile[[employee_id]]$education[[i]]$schoolName)}
    
    
    
    
    if(is.null(linkedin_profile[[employee_id]]$education[[i]]$fieldOfStudy)){
      
      fieldOfStudy = c(fieldOfStudy, NA)
    }
    
    else{fieldOfStudy = c(fieldOfStudy, linkedin_profile[[employee_id]]$education[[i]]$fieldOfStudy)}
    
    
    if(is.null(linkedin_profile[[employee_id]]$education[[i]]$degreeName)){
      
      degreeName = c(degreeName, NA)
      
    }
    
    else{degreeName = c(degreeName, linkedin_profile[[employee_id]]$education[[i]]$degreeName)}
    
  }
  
  education_data = data.frame(employee_id = rep(employee_id, length(schoolName)),
                              schoolName = schoolName,
                              fieldOfStudy = fieldOfStudy,
                              degreeName  = degreeName)
  
  return(education_data)
  
}


education_data = data.frame()

for (employee_id in names(linkedin_profile)){
  
  education_data = rbind(education_data,
                         education_extract(employee_id))
}


write.csv(education_data, "education_data.csv", row.names = FALSE)


### ============================ create skill data ============================

skill_extract = function(employee_id){
  skills = ""

  
  if(length(linkedin_profile[[employee_id]]$skills) > 0) {
    for (i in c(1:length(linkedin_profile[[employee_id]]$skills))){
      
      if(is.null(linkedin_profile[[employee_id]]$skills[[i]]$name)){
        
        skills = str_c(skills, "")
      
      }
      
      else{skills = str_c(skills, ", ", 
                          trimws(linkedin_profile[[employee_id]]$skills[[i]]$name))
      }
    }
  }
  
  skills = str_remove(skills, ", ")

  return(data.frame(employee_id = employee_id,
                    skills = skills))
  
}

skill_data = data.frame()

for (employee_id in names(linkedin_profile)){
  skill_data = rbind(skill_data, skill_extract(employee_id))
}


write.csv(skill_data, "skill_data.csv", row.names = FALSE)


### ================================= create language data =======================

language_extract = function(employee_id){
  
  language = c()
  proficiency = c()
  
  if(length(linkedin_profile[[employee_id]]$languages) > 0){
    
    for(i in c(1:length(linkedin_profile[[employee_id]]$languages))){
      
      
      if(is.null(linkedin_profile[[employee_id]]$languages[[i]]$name)){
        
        language = c(language, NA)
      }
      
      else{language = c(language, linkedin_profile[[employee_id]]$languages[[i]]$name)}
      
      
      if(is.null(linkedin_profile[[employee_id]]$languages[[i]]$proficiency)){
        
        proficiency = c(proficiency, NA)
      }
      
      else{proficiency = c(proficiency, linkedin_profile[[employee_id]]$languages[[i]]$proficiency)}
    
      
    }
    employee_id = rep(employee_id, length(language))
    
    return(data.frame(employee_id= employee_id,
                      language = language,
                      proficiency = proficiency))
    
  }
}
    
  
language_data = data.frame()

for (employee_id in names(linkedin_profile)){
  language_data = rbind(language_data, language_extract(employee_id))
}


write.csv(language_data, "language_data.csv", row.names = FALSE)

### ========================= processing picture data =================================

image_extract  = function(employee_id){
  
  if(is.null(linkedin_profile[[employee_id]]$displayPictureUr)){
    
    displayPictureUrl = NA
  }
  
  else{displayPictureUrl = linkedin_profile[[employee_id]]$displayPictureUr}
  
  if(is.null(linkedin_profile[[employee_id]]$img_400_400)){
    
    img_400_400 = NA
  }
  
  else{img_400_400 = linkedin_profile[[employee_id]]$img_400_400}
  
  if(is.null(linkedin_profile[[employee_id]]$img_800_800)){
    
    img_800_800 = NA
  }
  
  else{img_800_800 = linkedin_profile[[employee_id]]$img_800_800}
  
  
  url_400_400 = str_c(displayPictureUrl, img_400_400)
  
  url_800_800 = str_c(displayPictureUrl, img_800_800)
  
  return(data.frame(employee_id = employee_id,
                    url_400_400 = url_400_400,
                    url_800_800 = url_800_800))
}



image_data = data.frame()

for (employee_id in names(linkedin_profile)){
  image_data = rbind(image_data, image_extract(employee_id))
}



write.csv(image_data, "image_data.csv", row.names = FALSE)


## ============ create career progress variable ==============================


experience_data = read_csv("C:/Users/doduc/Github/Socialscience_bigdata_KUL/data_processing/experience_data.csv")

experience_data$X = row.names(experience_data)

experience_data$title = str_to_lower(experience_data$title)


## Firstly we should categorize the title into 5 category: Entry level, Lower management level, middle management level, upper management level, executive level
# Preprocess the title data

### 2 -gram tokenization for job title
data_processed <- experience_data %>%
  select(title) %>%
  mutate(title = tolower(title)) %>%
  unnest_tokens(input = title, output = "tokens", token = "ngrams", n = 2) %>% 
  mutate(tokens = str_replace_all(tokens, "[^[:alnum:]\\s]", ""))

# Load stopwords and additional custom words to remove
stop_words <- bind_rows(list(data.frame(word = stopwords("en"), stringsAsFactors = FALSE),
                             data.frame(word = c("skill", "skills"), stringsAsFactors = FALSE)))

# Remove stop words
data_processed <- data_processed %>%
  anti_join(stop_words, by = c("tokens" = "word"))

# Find the most frequent skills
top_title_2 <- data_processed %>%
  count(tokens, sort = TRUE) %>%
  top_n(100)



### 3 -gram tokenization for job title
data_processed <- experience_data %>%
  select(title) %>%
  mutate(title = tolower(title)) %>%
  unnest_tokens(input = title, output = "tokens", token = "ngrams", n = 3) %>% 
  mutate(tokens = str_replace_all(tokens, "[^[:alnum:]\\s]", ""))

# Load stopwords and additional custom words to remove
stop_words <- bind_rows(list(data.frame(word = stopwords("en"), stringsAsFactors = FALSE),
                             data.frame(word = c("skill", "skills"), stringsAsFactors = FALSE)))

# Remove stop words
data_processed <- data_processed %>%
  anti_join(stop_words, by = c("tokens" = "word"))

# Find the most frequent skills
top_title_3 <- data_processed %>%
  count(tokens, sort = TRUE) %>%
  top_n(100)


### words tokenization for job title
data_processed <- experience_data %>%
  select(title) %>%
  mutate(title = tolower(title)) %>%
  unnest_tokens(input = title, output = "tokens", token = "ngrams", n = 1) %>% 
  mutate(tokens = str_replace_all(tokens, "[^[:alnum:]\\s]", ""))

# Load stopwords and additional custom words to remove
stop_words <- bind_rows(list(data.frame(word = stopwords("en"), stringsAsFactors = FALSE),
                             data.frame(word = c("skill", "skills"), stringsAsFactors = FALSE)))

# Remove stop words
data_processed <- data_processed %>%
  anti_join(stop_words, by = c("tokens" = "word"))

# Find the most frequent skills
top_title_1 <- data_processed %>%
  count(tokens, sort = TRUE) %>%
  top_n(100)


### we would filter job title based on the following term

experience_data_filter = experience_data %>% filter(str_detect(title, ".*data.*")|
                                                      str_detect(title, ".* ai .*")|
                                                      str_detect(title, ".*business.*intelligence.*")|
                                                      str_detect(title, ".* bi .*")|
                                                      str_detect(title, ".*developer.*")|
                                                      str_detect(title, ".*machine.*learning.*")|
                                                      str_detect(title, ".*risk.*")|
                                                      str_detect(title, ".*artificial.*intelligence.*")|
                                                      str_detect(title, ".*software.*")|
                                                      str_detect(title, ".*computer.*vision.*")|
                                                      str_detect(title, ".*deep.*learning.*")|
                                                      str_detect(title, ".*full.*stack.*")|
                                                      str_detect(title, ".*natural.*language.*")|
                                                      str_detect(title, ".* nlp .*")|
                                                      str_detect(title, ".*statistic.*")|
                                                      str_detect(title, ".* mlops .*")|
                                                      str_detect(title, ".*quantitative.*")|
                                                      str_detect(title, ".*model.*")|
                                                      str_detect(title, ".*actuarial.*")|
                                                      str_detect(title, ".*business.*analyst.*")|
                                                      str_detect(title, ".*business.*analytics.*")|
                                                      str_detect(title, ".*ml.*engineer.*"))




experience_data_high_level =  experience_data %>% filter(str_detect(title, ".* ceo .*")|
                                                         str_detect(title, ".* cto .*")|
                                                         str_detect(title, ".*founder.*")|
                                                         str_detect(title, ".*fouding.*")|
                                                         str_detect(title, ".*chief.*")|
                                                         str_detect(title, ".*owner.*")|
                                                         str_detect(title, ".*president.*")|
                                                         str_detect(title, ".*director.*")|
                                                         str_detect(title, ".*manager.*")|
                                                         str_detect(title, ".*supervisor.*")|
                                                         str_detect(title, ".*lead.*")|
                                                         str_detect(title, ".*head.*")|
                                                         str_detect(title, ".*expert.*")|
                                                         str_detect(title, ".*principal.*")|
                                                         str_detect(title, ".*senior.*")|
                                                         str_detect(title, ".* sr .*")) 
                                                      
                                                                                                       




experience_data_high_level = experience_data_high_level %>% 
  inner_join(experience_data_filter %>% distinct(employee_id),
             by = "employee_id")




experience_data_filter = experience_data_filter %>% bind_rows(experience_data_high_level)


experience_data_filter = experience_data_filter[!duplicated(experience_data_filter$X), ]


## we need to exclude the jobs related to academic field like phd, teaching, lecture and also intern, working student job


experience_data_filter$industry = str_to_lower(experience_data_filter$industry)



experience_data_filter = experience_data_filter %>% 
  filter(!str_detect(title, "student") &
           !str_detect(title, ".* phd .*") &
           !str_detect(title, "intern") &
           !str_detect(title, "teaching") &
           !str_detect(title, "lecture")) %>% 
  filter(industry != "research" | is.na(industry))



## we also delete the job title with freelance term

experience_data_filter = experience_data_filter %>% 
  filter(!str_detect(title, "freelance"))


### we need to categorize job title


experience_data_filter = experience_data_filter %>% 
  mutate(job_level = case_when(str_detect(title, ".*senior.*")|
                                 str_detect(title, ".* sr .*") ~ 1,
                               (str_detect(title, ".*lead.*") & !str_detect(title, ".*lead to.*"))|
                                 str_detect(title, ".*principal.*")|
                                 str_detect(title, ".*expert.*") ~ 2,
                               str_detect(title, ".*head.*")|
                                 str_detect(title, ".*supervisor.*")|
                                 str_detect(title, ".*manager.*") ~ 3,
                               str_detect(title, ".* ceo .*")|
                                 str_detect(title, ".* cto .*")|
                                 str_detect(title, ".*founder.*")|
                                 str_detect(title, ".*fouding.*")|
                                 str_detect(title, ".*chief.*")|
                                 (str_detect(title, ".*owner.*") & !str_detect(title, ".*product.*owner*"))|
                                 str_detect(title, ".*president.*")|
                                 str_detect(title, ".*director.*") ~ 4,
                               T ~ 0))


## Load information of employer (subdata)


employer_data_add =  read_json("C:/Users/doduc/OneDrive - KU Leuven/My knowledge/Master of Statistics_KUL/Collecting big data/SS_bigdata/company_profiles.json")



companyurn_add  = names(employer_data_add)

staffCount_vec = c()

for (i in companyurn_add){
  if(is.null(employer_data_add[[i]]$staffCount)){
    
    staffCount = NA
  }
  
  else{staffCount = employer_data_add[[i]]$staffCount}
  
  staffCount_vec = c(staffCount_vec, staffCount)
}


num_employee_add = data.frame(companyurn = companyurn_add,
                              staffCount = staffCount_vec)


num_employee_add = num_employee_add %>% 
  mutate(employer_cate_add = case_when(staffCount < 100 ~  0,
                                   staffCount <= 999 & staffCount >= 100 ~  1,
                                   staffCount <= 9999 & staffCount >= 1000 ~  2,
                                   staffCount >= 10000 ~  3))



num_employee_add$companyurn = as.numeric(num_employee_add$companyurn)


experience_data_filter = experience_data_filter %>% 
  left_join(num_employee_add %>% select(companyurn, employer_cate_add), 
            by = "companyurn")


experience_data_filter = experience_data_filter %>% 
  mutate(employer_cate_final = case_when(is.na(employer_cate) ~ employer_cate_add,
                                         T ~ employer_cate))



## In case of NA employee category, we decide to assign 0 category

experience_data_filter = experience_data_filter %>% 
  mutate(employer_cate_final = case_when(is.na(employer_cate_final) ~ 0,
                                         T ~ employer_cate_final))


## we create stage career variable

experience_data_filter = experience_data_filter %>% 
  mutate(stage_career = case_when(job_level == 0 & employer_cate_final == 0 ~ 0,
                                  (job_level == 0 & employer_cate_final == 1) |
                                    (job_level == 1 & employer_cate_final == 0) ~ 1,
                                  (job_level == 0 & employer_cate_final == 2) |
                                    (job_level == 1 & employer_cate_final == 1) |
                                    (job_level == 2 & employer_cate_final == 0) ~ 2,
                                  (job_level == 0 & employer_cate_final == 3) |
                                    (job_level == 1 & employer_cate_final == 2) |
                                    (job_level == 2 & employer_cate_final == 1) |
                                    (job_level == 3 & employer_cate_final == 0) ~ 3,
                                  (job_level == 1 & employer_cate_final == 3) |
                                    (job_level == 2 & employer_cate_final == 2) |
                                    (job_level == 3 & employer_cate_final == 1) |
                                    (job_level == 4 & employer_cate_final == 0) ~ 4,
                                  (job_level == 2 & employer_cate_final == 3) |
                                    (job_level == 3 & employer_cate_final == 2) |
                                    (job_level == 4 & employer_cate_final == 1) ~ 5,
                                  (job_level == 3 & employer_cate_final == 3) |
                                    (job_level == 4 & employer_cate_final == 2) ~ 6,
                                  (job_level == 4 & employer_cate_final == 3) ~ 7))




experience_data_filter  = experience_data_filter %>% 
  arrange(employee_id, stage_career, start_date)



career_progress_extract = function(id){
  
  experience_user_data = experience_data_filter %>% filter(employee_id == id)
  
  min_stage = min(experience_user_data$stage_career)
  
  duration_vec = c(id, min_stage)
  
  for (i in c(1:7)){
    if (i < min_stage){
      duration = NA
    }
    
    if(i == min_stage){
      duration = 0
    }
    
    if(i > min_stage & i %in% unique(experience_user_data$stage_career)){
      
      min_date_min_stage = min(experience_user_data %>% filter(stage_career == min_stage) %>% .$start_date)
      min_date_stage = min(experience_user_data %>% filter(stage_career == i) %>% .$start_date)
      duration = difftime(min_date_stage, min_date_min_stage , units = "days")
      
    }
    
    if(i > min_stage & !(i %in% unique(experience_user_data$stage_career))){
      
      duration = NA
      
    }
    
    duration_vec = c(duration_vec, duration)
  }
  
  return(duration_vec)
  
}

career_progress_data = data.frame()

for(id in unique(experience_data_filter$employee_id)){
  career_progress_data = rbind(career_progress_data, 
                               career_progress_extract(id))
}


names(career_progress_data) = c("employee_id", "min_stage",
                                "duration_1", "duration_2", 
                                "duration_3", "duration_4",
                                "duration_5", "duration_6",
                                "duration_7")



for (col in names(career_progress_data)[-1]){
  career_progress_data[col] = as.numeric(career_progress_data[[col]])
}


