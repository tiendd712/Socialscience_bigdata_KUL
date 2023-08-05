
# load library
library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

# load data
ess_data = read_csv("ESS-Data-Wizard-subset-2023-08-03.csv")
var_pick = read_excel("var_pick.xlsx", sheet = 1) 


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

# Select rows for West European countries
west_europe_countries <- c("BE", "DE", "FR", "NL")
ess_data_1 <- ess_data[ess_data$cntry %in% west_europe_countries, ]

# Calculate NA Percentages
na_threshold <- 0.3  
na_percentages <- colMeans(is.na(ess_data_1))
# Delete columns
cleaned_data <- ess_data_1[, na_percentages <= na_threshold]

# Remove NA values
cleaned_data <- cleaned_data %>% na.omit()



# List of columns to convert to categorical (all but "noimbro", "agea", "eduyrs")
columns_to_convert <- setdiff(names(cleaned_data), c("noimbro", "agea", "eduyrs"))

# Convert selected columns to categorical
cleaned_data <- cleaned_data %>%
  mutate(across(all_of(columns_to_convert), as.factor))

# Data Summary
summary(cleaned_data)
str(cleaned_data)

## EDA

## Distribution of Immigrant Perception by Country:
# Create a custom color palette
color_palette <- brewer.pal(12, "Paired")

# Create the bar plot with counts of each category by country
ggplot(cleaned_data, aes(x = imwbcnt, fill = cntry)) +
  geom_bar(position = "dodge") +
  labs(title = "Counts of Immigrant Perception Categories by Country", x = "Immigrant Perception", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = color_palette) +
  theme(legend.position = "right")

# Create scatter plots for each continuous variable
par(mfrow = c(1, 3)) 

ggplot(cleaned_data, aes(x = cntry, y = noimbro)) +
  geom_boxplot() +
  labs(title = "Box Plot of noimbro by Country", x = "Country", y = "noimbro") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(cleaned_data, aes(x = cntry, y = agea)) +
  geom_boxplot() +
  labs(title = "Box Plot of noimbro by Country", x = "Country", y = "noimbro") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(cleaned_data, aes(x = cntry, y = eduyrs)) +
  geom_boxplot() +
  labs(title = "Box Plot of noimbro by Country", x = "Country", y = "noimbro") +
  theme_minimal() +
  theme(legend.position = "none")


# Create bar plots for each categorical variable
# Select only the categorical columns
categorical_columns <- cleaned_data %>%
  select(-c("noimbro", "agea", "eduyrs"))
names(categorical_columns)

bar_plots_cat <- function(cat) {
  ggplot(cleaned_data, aes(x = cat, fill = cntry)) +
    geom_bar(position = position_dodge(preserve = "single")) +
    labs(title = paste("Bar Plot of", cat), x = cat, y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = color_palette) +
    theme(legend.position = "right")
}

# Country Distribution
bar_plots_cat(categorical_columns$cntry)

# TV watching, news/politics/current affairs on average weekday
bar_plots_cat(categorical_columns$tvpol)

# Most people can be trusted or you can't be too careful
bar_plots_cat(categorical_columns$ppltrst)

# How interested in politics
bar_plots_cat(categorical_columns$polintr)

# Placement on left right scale
bar_plots_cat(categorical_columns$lrscale)

# How satisfied with life as a whole
bar_plots_cat(categorical_columns$stflife)

# How satisfied with present state of economy in country
bar_plots_cat(categorical_columns$stfeco)

# Gays and lesbians free to live life as they wish
bar_plots_cat(categorical_columns$freehms)

# Immigrants make country worse or better place to live
bar_plots_cat(categorical_columns$imwbcnt)

# How happy are you
bar_plots_cat(categorical_columns$happy)

# Respondent or household member victim of burglary/assault last 5 years
bar_plots_cat(categorical_columns$crmvct)

# Feeling of safety of walking alone in local area after dark
bar_plots_cat(categorical_columns$aesfdrk)

# Belonging to particular religion or denomination
bar_plots_cat(categorical_columns$rlgblg)

# How religious are you
bar_plots_cat(categorical_columns$rlgdgr)

# Born in country
bar_plots_cat(categorical_columns$brncntr)

# Belong to minority ethnic group in country
bar_plots_cat(categorical_columns$blgetmg)

# People of minority race/ethnic group in current living area
bar_plots_cat(categorical_columns$acetalv)

# Qualification for immigration: good educational qualifications
bar_plots_cat(categorical_columns$qfimedu)

# Qualification for immigration: speak country's official language
bar_plots_cat(categorical_columns$qfimlng)

# Qualification for immigration: work skills needed in country
bar_plots_cat(categorical_columns$qfimwsk)

# Qualification for immigration: committed to way of life in country
bar_plots_cat(categorical_columns$qfimcmt)

# Immigrants take jobs away in country or create new jobs
bar_plots_cat(categorical_columns$imtcjob)

# Taxes and services: immigrants take out more than they put in or less
bar_plots_cat(categorical_columns$imbleco)

# Immigrants make country's crime problems worse or better
bar_plots_cat(categorical_columns$imwbcrm)

# Different race or ethnic group: have any close friends
bar_plots_cat(categorical_columns$dfegcf)

# Different race or ethnic group: contact, how bad or good
bar_plots_cat(categorical_columns$dfeghbg)

# Feel close to country
bar_plots_cat(categorical_columns$fclcntr)

# Some races or ethnic groups: born less intelligent
bar_plots_cat(categorical_columns$smegbli)

# Some races or ethnic groups: born harder working
bar_plots_cat(categorical_columns$smegbhw)

# Some cultures: much better or all equal
bar_plots_cat(categorical_columns$smctmbe)

# Gender
bar_plots_cat(categorical_columns$gndr)

# Legal marital status
bar_plots_cat(categorical_columns$marsts)

# Children living at home or not
bar_plots_cat(categorical_columns$chldhm)

# Highest level of education, ES - ISCED
bar_plots_cat(categorical_columns$eisced)

# Important that people are treated equally and have equal opportunities
bar_plots_cat(categorical_columns$ipeqopt)

# Important to live in secure and safe surroundings
bar_plots_cat(categorical_columns$impsafe)




