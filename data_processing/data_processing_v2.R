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


### =========================== create number of follower data ============================= 

follower_extract = function(employee_id){
  list_connect = linkedin_profile[[employee_id]]$network
  if(is.null(list_connect$followersCount)){follower_count = NA}
  else{follower_count = list_connect$followersCount}
  return(follower_count)
}

follower_count = c()
employee_vec = c()


for (employee_id in names(linkedin_profile)){
  follower_count = c(follower_count, follower_extract(employee_id))
  employee_vec = c(employee_vec, employee_id)
}

follower_data = data.frame(employee_id = employee_vec,
                           follower_count = follower_count)



write.csv(follower_data, "C:/Users/doduc/Github/Socialscience_bigdata_KUL/data_processing/follower_data.csv", row.names = FALSE)


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
  mutate(employer_cate = case_when(
    employee_range_start > 10000 ~  1,
    T ~ 0))


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
  start_year = c()
  start_month = c()
  end_year = c()
  end_month = c()
  
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
    
    
    if(is.null(linkedin_profile[[employee_id]]$education[[i]]$timePeriod$startDate$year)){
      
      start_year = c(start_year, NA)
      
    }
    
    else{start_year = c(start_year, linkedin_profile[[employee_id]]$education[[i]]$timePeriod$startDate$year)}
    
    
    if(is.null(linkedin_profile[[employee_id]]$education[[i]]$timePeriod$startDate$month)){
      
      start_month = c(start_month, NA)
      
    }
    
    else{start_month = c(start_month, linkedin_profile[[employee_id]]$education[[i]]$timePeriod$startDate$month)}
    
    
    if(is.null(linkedin_profile[[employee_id]]$education[[i]]$timePeriod$endDate$year)){
      
      end_year = c(end_year, NA)
      
    }
    
    else{end_year = c(end_year, linkedin_profile[[employee_id]]$education[[i]]$timePeriod$endDate$year)}
    
    
    
    if(is.null(linkedin_profile[[employee_id]]$education[[i]]$timePeriod$endDate$month)){
      
      end_month = c(end_month, NA)
      
    }
    
    else{end_month = c(end_month, linkedin_profile[[employee_id]]$education[[i]]$timePeriod$endDate$month)}
    
    
    
  }
  
  education_data = data.frame(employee_id = rep(employee_id, length(schoolName)),
                              schoolName = schoolName,
                              fieldOfStudy = fieldOfStudy,
                              degreeName  = degreeName,
                              start_year = start_year,
                              start_month = start_month,
                              end_year = end_year,
                              end_month = end_month)
  
  return(education_data)
  
}


education_data = data.frame()

for (employee_id in names(linkedin_profile)){
  
  education_data = rbind(education_data,
                         education_extract(employee_id))
}


write.csv(education_data, "C:/Users/doduc/Github/Socialscience_bigdata_KUL/data_processing/education_data.csv", row.names = FALSE)


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




### ========================= create summary data =================================

summary_extract  = function(employee_id){
  
  if(is.null(linkedin_profile[[employee_id]]$summary)){
    
    summary = NA
  }
  
  else{summary = linkedin_profile[[employee_id]]$summary}
  
  
 
  return(data.frame(employee_id = employee_id,
                    summary = summary))
}



summary_data = data.frame()

for (employee_id in names(linkedin_profile)){
  summary_data = rbind(summary_data, summary_extract(employee_id))
}



write.csv(summary_data, "summary_data.csv", row.names = FALSE)



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

experience_data_job = experience_data %>% filter(str_detect(title, ".*data.*")|
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



experience_data_job$companyname = str_to_lower(experience_data_job$companyname)
experience_data_job$industry = str_to_lower(experience_data_job$industry)

### we would exclude the highest position level and self employee, freelance


experience_data_po = experience_data_job %>% filter(str_detect(title, ".*product.*owner*"))

experience_data_filter = experience_data_job %>% 
  filter(!str_detect(title, ".* ceo .*") &
           !str_detect(title, ".* cto .*") &
           !str_detect(title, ".*founder.*") &
           !str_detect(title, ".*founding.*") &
           !str_detect(title, ".*chief.*") &
           !str_detect(title, ".*owner.*")&
           !str_detect(title, ".*president.*") &
           !str_detect(companyname, ".*self.*employ.*")&
           !str_detect(companyname, ".*freelance.*")) 



experience_data_filter = experience_data_filter %>% bind_rows(experience_data_po)


## we need to exclude the jobs related to academic field like phd, teaching, lecture and also intern, working student job



experience_data_filter = experience_data_filter %>% 
  filter(!str_detect(title, "student") &
           !str_detect(title, ".* phd .*") &
           !str_detect(title, "intern") &
           !str_detect(title, "teaching") &
           !str_detect(title, "teacher") &
           !str_detect(title, "lecture")) %>% 
  filter(industry != "research" | is.na(industry))



## we also delete the job title with freelance term

experience_data_filter = experience_data_filter %>% 
  filter(!str_detect(title, "freelance"))


### we need to categorize job title
         

experience_data_filter = experience_data_filter %>% 
  mutate(job_level_1 = case_when((str_detect(title, ".*lead.*") & !str_detect(title, ".*lead to.*"))|
                                   str_detect(title, ".*principal.*") |
                                   str_detect(title, ".*senior.*") |
                                   str_detect(title, ".* sr .*") ~ 1,
                                 T ~ 0),
         job_level_2 = case_when(str_detect(title, ".*head.*")|
                                   str_detect(title, ".*supervisor.*")|
                                   str_detect(title, ".*manager.*") |
                                   str_detect(title, ".*director.*") |
                                   str_detect(title, ".*expert.*") ~ 1,
                                 T ~ 0))





career_progress_extract = function(id){
  
  experience_user_data = experience_data_filter %>% filter(employee_id == id & !is.na(start_date))
  
  job_level_1 = case_when(max(experience_user_data$job_level_1) == 0 ~ 0, T ~ 1)
  
  job_level_2 = case_when(max(experience_user_data$job_level_2) == 0 ~ 0, T ~ 1)
  
  max_date = max(experience_user_data[experience_user_data$start_date == max(experience_user_data$start_date), "end_date", drop = T])
  
  min_date = min(experience_user_data$start_date)
  
  if (is.na(max_date)){
    duration = difftime(Sys.Date(), min_date, units = "days")
    
  }
  
  else{duration = difftime(max_date, min_date, units = "days")}
  
 return(data.frame(employee_id = id,
                   promote_level_1 = job_level_1,
                   promote_level_2 = job_level_2,
                   time_work = as.numeric(duration)))
  
}

career_progress_data = data.frame()

for (id in unique(experience_data_filter$employee_id)){
  
  career_progress_data = rbind(career_progress_data, career_progress_extract(id))
}


career_progress_data = career_progress_data %>% 
  mutate(promote_general = case_when(promote_level_1 == 1 | promote_level_2 == 1 ~ 1,
                                     T ~ 0))


### create variable indicate that the employee have an intern or not


experience_data_intern = experience_data_job %>% 
  filter(str_detect(title, ".*student.*")|
           str_detect(title, ".*intern.*")) %>% distinct(employee_id) %>% 
  mutate(intern = 1)


career_progress_data = career_progress_data %>%
  left_join(experience_data_intern, by = "employee_id") %>% 
  mutate(intern = case_when(is.na(intern) ~ 0,
                            T ~ 1))


### create variable indicate the number of job for each employee_id:

number_job_data = experience_data_filter %>% 
  group_by(employee_id) %>% 
  summarise(num_job = n())

### create variable indicate the number of company for each employee_id:

num_company_data = experience_data_filter %>% 
  mutate(companyurn_fix = case_when(is.na(companyurn) ~ companyname,
                                    T ~ as.character(companyurn))) %>% 
  group_by(employee_id) %>% 
  summarise(num_company = n_distinct(companyurn_fix))


### create data model

data_model = career_progress_data %>% 
  inner_join(number_job_data, by = "employee_id") %>% 
  inner_join(num_company_data, by = "employee_id")


write.csv(data_model, "C:/Users/doduc/Github/Socialscience_bigdata_KUL/data_processing/data_model.csv", row.names = FALSE)
