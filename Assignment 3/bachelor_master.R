library(tidyverse)
library(dplyr)

#Do they do Masters after Bachelors?

# Read the education data from the CSV file
edu <- read.csv("education_data.csv")

# Filter education data to exclude degrees completed after 2023
edu <- edu %>%
  filter(is.na(end_year) | end_year < 2023)

# Clean the degree names
edu <- edu %>%
  mutate(clean_degree = gsub("[^A-Za-z0-9]", "", degreeName))


# Map degree names to bachelor's, master's, and other categories
edu <- edu %>%
  mutate(clean_degree = case_when(
    grepl("BA|BS|BE|BICT", clean_degree) ~ "bachelor",
    grepl("bachelor|bsc|btech|bcom|bcs|beng|engineer|ingenieur|undergraduate|graduate|graduaat|bacharelado", tolower(clean_degree)) ~ "bachelor",
    grepl("MA|MS", clean_degree) ~ "master",
    grepl("msc|master|mba|mphil|postgrad|licentiate", tolower(clean_degree)) & !grepl("premaster", tolower(clean_degree)) ~ "master",
    grepl("doctor|postdoc|phd|dr", tolower(clean_degree))   & !grepl("medicaldoctor", tolower(clean_degree)) ~ "phd",
    grepl("exchange", tolower(clean_degree)) ~ NA_character_,
    TRUE ~ NA_character_
  ))

# Clean the field of study names
edu <- edu %>%
  mutate(degree_from_fieldofstudy = gsub("[^A-Za-z0-9 ]", "", fieldOfStudy))


# Map field of study names to bachelor's, master's, and other categories
edu <- edu %>%
  mutate(degree_from_fieldofstudy = case_when(
    grepl("BA|BS|BE|BICT", clean_degree) ~ "bachelor",
    grepl("bsc|bachelor|btech|bcom|bcs|beng|undergraduate|graduate|graduaat|bacharelado", tolower(degree_from_fieldofstudy)) ~ "bachelor",
    grepl("MA|MS", degree_from_fieldofstudy) ~ "master",
    grepl("master|mba|msc|mphil|postgrad", tolower(degree_from_fieldofstudy)) & !grepl("premaster", tolower(degree_from_fieldofstudy)) ~ "master",
    grepl("doctor|postdoc|phd|dr", tolower(degree_from_fieldofstudy)) & !grepl("medicaldoctor", tolower(degree_from_fieldofstudy)) ~ "phd",
    grepl("exchange", clean_degree) ~ NA_character_,
    TRUE ~ NA_character_
  ))

# Consolidate the clean degree names
edu <- edu %>%
  mutate(clean_degree = coalesce(clean_degree, degree_from_fieldofstudy))

# Filter individuals where master's degree starts before the bachelor's degree
edu_no <- edu %>%
  group_by(employee_id) %>%
  mutate(has_bachelor = any(clean_degree == "bachelor"),
         has_master = any(clean_degree == "master"),
         master_before_bachelor = has_bachelor & has_master & min(start_year[clean_degree == "master"]) < min(start_year[clean_degree == "bachelor"])) %>%
  ungroup() %>%
  filter(master_before_bachelor)

# leave out those people
edu1 <- anti_join(edu, edu_no, join_by(employee_id))


# Filter people whose field of study is relevant to the data and software field
relevant_fields <- c("AI|IT|ICT",
                     "math|machine learning|ml|wiskunde|stat|web|informatic|comput|quantitative|informatik|informatica|",
                     "data|analytics|artificial intelligence|nlp|natural language processing|software|actuarial|actuary|deep learning|reinforcement learning",
                     "business intelligence|business engineer|programming|system|robot|information technology|information management")

# Filter the education data to include only people with a relevant field of study during bachelor
relevant_edu <- edu1 %>%
  filter(!is.na(clean_degree)) %>%
  mutate(clean_field = gsub("[^A-Za-z0-9 ]", "", fieldOfStudy),
         clean_field = case_when(
           grepl(relevant_fields[1], clean_field, ignore.case = FALSE) |
             grepl(paste(relevant_fields[2:4], collapse = ""), clean_field, ignore.case = TRUE) ~ 1,
           TRUE ~ 0
         )) %>% 
  mutate(field_from_degree = gsub("[^A-Za-z0-9 ]", "", degreeName),
         field_from_degree = case_when(
           grepl(relevant_fields[1], field_from_degree, ignore.case = FALSE) |
             grepl(paste(relevant_fields[2:4], collapse = ""), field_from_degree, ignore.case = TRUE) ~ 1,
           TRUE ~ 0
         )) %>%
  group_by(employee_id) %>%
  mutate(has_relevant_field = as.integer(any(clean_field == 1 | field_from_degree == 1)),
         bachelor_relevant_field = ifelse(clean_degree == "bachelor" & has_relevant_field == 1, 1, 0)) %>% 
  filter(has_relevant_field == 1 & sum(bachelor_relevant_field) > 0) %>%
  select(employee_id, schoolName, fieldOfStudy, degreeName, start_year, end_year, clean_degree)

# Create a dataset including all rows from edu that are not present in relevant_edu
other_edu <- anti_join(edu1, relevant_edu, by = "employee_id")


# Find each person who has done bachelor/master with a bachelor in the field
relevant_processed <- relevant_edu %>%
  group_by(employee_id) %>%
  summarise(bache_mast = case_when(
    "master" %in% clean_degree ~ "master",
    "bachelor" %in% clean_degree ~ "bachelor",
    TRUE ~ NA_character_
  ))

# Filter to include only bachelor and master degrees
bachelor_master_1 <- relevant_processed %>%
  filter(bache_mast %in% c("bachelor", "master"))

# Find each person who has done bachelor/master with a bachelor not in the field
irr_processed <- other_edu %>%
  group_by(employee_id) %>%
  summarise(bache_mast = case_when(
    "master" %in% clean_degree ~ "master",
    "bachelor" %in% clean_degree ~ "bachelor",
    TRUE ~ NA_character_
  ))

# Filter to include only bachelor and master degrees
bachelor_master_2 <- irr_processed %>%
  filter(bache_mast %in% c("bachelor", "master"))


# Combine the bache_mast column from bachelor_master_1 and bachelor_master_2 into a single table
bachefield_yes <- table(bachelor_master_1$bache_mast)
bachefield_no <- table(bachelor_master_2$bache_mast)
combined_table <- cbind(bachefield_yes, bachefield_no)
total <- combined_table["bachelor", ] + combined_table["master", ]
mast_percentage <- round(combined_table["master", ] / total, 3)


# Print the combined table
print(combined_table)
print(mast_percentage)

# Perform the likelihood ratio test
chisq_test <- chisq.test(combined_table)
print(chisq_test)


# We can see that people are less likely to persue a master's degree 
# when they hold a bachelor's degree in ds/ai field. 
# Hypothesis
# Individuals with a bachelor's degree in DS/AI may find lucrative job opportunities and industry 
# demand even without pursuing a master's degree

