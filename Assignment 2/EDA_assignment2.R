
# load library
library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(gplots)

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
# west_europe_countries <- c("BE", "DE", "FR", "NL")
# ess_data_1 <- ess_data[ess_data$cntry %in% west_europe_countries, ]

# Calculate NA Percentages
na_threshold <- 0.3  
na_percentages <- colMeans(is.na(ess_data))
# Delete columns
cleaned_data <- ess_data[, na_percentages <= na_threshold]

# Remove NA values
cleaned_data <- cleaned_data %>% na.omit()


# Define the mapping of countries to regions
country_to_region <- c(
  "AL" = "Eastern EU", "AT" = "Western EU", "BE" = "Western EU", "BG" = "Eastern EU",
  "CH" = "Western EU", "CY" = "Eastern EU", "CZ" = "Eastern EU", "DE" = "Western EU",
  "DK" = "Western EU", "EE" = "Eastern EU", "ES" = "Western EU", "FI" = "Western EU",
  "FR" = "Western EU", "GB" = "Western EU", "GE" = "Eastern EU", "GR" = "Eastern EU",
  "HR" = "Eastern EU", "HU" = "Eastern EU", "IE" = "Western EU", "IS" = "Western EU",
  "IL" = "Eastern EU", "IT" = "Western EU", "LT" = "Eastern EU", "LU" = "Western EU",
  "LV" = "Eastern EU", "ME" = "Eastern EU", "MK" = "Eastern EU", "NL" = "Western EU",
  "NO" = "Western EU", "PL" = "Eastern EU", "PT" = "Western EU", "RO" = "Eastern EU",
  "RS" = "Eastern EU", "RU" = "Eastern EU", "SE" = "Western EU", "SI" = "Eastern EU",
  "SK" = "Eastern EU", "TR" = "Eastern EU", "UA" = "Eastern EU", "XK" = "Eastern EU"
)

# Create the new categorical column 'Region' based on the mapping
cleaned_data$region <- factor(country_to_region[cleaned_data$cntry])



# Convert specific variables to categorical
columns_to_cate <- c("crmvct", "rlgblg", "brncntr", "blgetmg", "smegbli",
                        "smegbhw", "smctmbe", "gndr", "chldhm", "eisced")

cleaned_data[columns_to_cate] <- lapply(cleaned_data[columns_to_cate], as.factor)

# Convert specific variables to numeric
columns_to_nume <- c("tvpol", "ppltrst", "polintr", "lrscale", "stflife", "stfeco",
                        "freehms", "imwbcnt", "happy", "aesfdrk", "rlgdgr", "acetalv",
                        "noimbro", "qfimedu", "qfimlng", "qfimwsk", "qfimcmt", "imtcjob",
                        "imbleco", "imwbcrm", "dfegcf", "dfeghbg", "fclcntr", "agea",
                        "eduyrs", "ipeqopt", "impsafe")

cleaned_data[columns_to_nume] <- lapply(cleaned_data[columns_to_nume], as.numeric)

# Data Summary
summary(cleaned_data)
str(cleaned_data)


## EDA

## Distribution of Immigrant Perception by Country:
# Create a custom color palette
color_palette <- brewer.pal(12, "Paired")

# Create the bar plot with counts of each category by country
ggplot(cleaned_data, aes(x = imwbcnt, fill = region)) +
  geom_bar() +
  labs(title = "Counts of Immigrant Perception Categories by Region", x = "Immigrant Perception", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = color_palette) +
  theme(legend.position = "right")


# Create a correlation matrix and heatmap
cor_matrix <- cor(cleaned_data[, columns_to_nume], use = "pairwise.complete.obs")

heatmap.2(cor_matrix, 
          col = colorRampPalette(c("blue", "white", "red"))(100),
          trace = "none", # Remove color key
          key = TRUE, keysize = 1, key.title = NA,
          main = "Correlation Heatmap of Numeric Columns",
          xlab = "Numeric Columns",
          ylab = "Numeric Columns",
          cexRow = 0.6, cexCol = 0.6,
          symm = TRUE, # Show symmetric plot
          density.info = "none") # Remove density plot



# Create box plots for each continuous variable
par(mfrow = c(1, 3)) 

ggplot(cleaned_data, aes(x = region, y = noimbro)) +
  geom_boxplot() +
  labs(title = "Box Plot of noimbro by Country", x = "Country", y = "Of every 100 people in country how many born outside country") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(cleaned_data, aes(x = region, y = agea)) +
  geom_boxplot() +
  labs(title = "Box Plot of noimbro by Country", x = "Country", y = "Age of respondent") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(cleaned_data, aes(x = region, y = eduyrs)) +
  geom_boxplot() +
  labs(title = "Box Plot of noimbro by Country", x = "Country", y = "Years of full-time education completed") +
  theme_minimal() +
  theme(legend.position = "none")

## To be done


# Create bar plots for each categorical variable
# Select only the categorical columns
categorical_columns <- cleaned_data %>%
  select(all_of(columns_to_cate))
names(categorical_columns)

bar_plots_cat <- function(cat) {
  ggplot(cleaned_data, aes(x = cat, fill = region)) +
    geom_bar(position = position_dodge(preserve = "single")) +
    labs(title = paste("Bar Plot of", cat), x = cat, y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = color_palette) +
    theme(legend.position = "right")
}


# Belong to minority ethnic group in country (1: Yes, 0: No)
bar_plots_cat(categorical_columns$blgetmg)
### most respondents do not belong to minority ethnic group in country


# Some races or ethnic groups: born less intelligent (1: Yes, 0: No)
bar_plots_cat(categorical_columns$smegbli)
### most respondents do not believe that some races or ethnic groups are born less intelligent


# Some races or ethnic groups: born harder working (1: Yes, 0: No)
bar_plots_cat(categorical_columns$smegbhw)
### most respondents from BE, DE, NL do not believe some races or ethnic groups are born harder working,
### while in FR, the spread of answers is relatively balanced


# Some cultures: much better or all equal (1:	Some cultures are much better than others
# 2:	All cultures are equal)
bar_plots_cat(categorical_columns$smctmbe)
### same as the previous one


# Gender (1	Male, 2	Female)
bar_plots_cat(categorical_columns$gndr)
### the spread of respondents' gender is relatively balanced


# Children living at home or not (1: Yes, 0: No)
bar_plots_cat(categorical_columns$chldhm)
### more respondents have children living at home than those who answered no


# Highest level of education, ES - ISCED 
bar_plots_cat(categorical_columns$eisced)



