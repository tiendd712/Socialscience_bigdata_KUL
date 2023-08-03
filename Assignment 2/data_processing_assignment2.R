library(tidyverse)
library(readxl)


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



