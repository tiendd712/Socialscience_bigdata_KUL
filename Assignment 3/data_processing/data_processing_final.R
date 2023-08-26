### load data
linkedin_profile =  read_json("all_profiles.json")

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


connection_data = connection_data %>% 
  mutate(connection_type = case_when(connection_count >= 500 ~ 1,
                                     T ~ 0))



### ========================= create experience data ===========================


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



experience_data$companyurn = as.numeric(unlist(str_extract_all(experience_data$companyurn, "\\d+")))



## exclude the profile having less than 20 connection (suspect clone profile)
experience_data = experience_data %>% 
  filter(!(employee_id %in% (connection_data %>% filter(connection_count < 20) %>% .$employee_id)))


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
