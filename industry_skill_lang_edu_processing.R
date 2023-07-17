library(tidyverse)
library(dplyr)
library(textplot)
library(wordcloud)
library(scales)
library(tm)
library(topicmodels)
library(quanteda)
library(spacyr)
library(tidytext)
library(stringr)
library(textstem)
library(kableExtra)
library(LDAvis)
library(lubridate)



# Load data ---------------------------------------------------------------


setwd("/Users/anhphuong/Documents/KUL/Collecting and Analyzing Data/Socialscience_bigdata_KUL/data_processing")
skill = read_csv("skill_data.csv")
exp = read_csv("experience_data.csv")
dim(exp)


# Most common industries --------------------------------------------------

industry_counts <- exp %>%
  group_by(employee_id, industry) %>%
  summarise(count = n_distinct(industry)) %>%
  group_by(industry) %>%
  count() %>%
  arrange(desc(n))

wordcloud(words = industry_counts$industry[1:30], freq = industry_counts$n[1:30],
          scale = c(1.5, 0.8), random.order = FALSE, colors = brewer.pal(8, "Set2"), min.freq = 1)


# Topic modeling on job description ---------------------------------------

# Remove symbol and NA lines
description_df <- data.frame(text = exp$description)
symbols_to_remove <- c("\u2022", "\u2023", "\u25E6", "\u2043", "\u2219", "\u9642", "\u65039")
description_df$text <- gsub(paste0("(?!", symbols_to_remove, ")\\P{L}+", collapse = "|"), " ", description_df$text, perl = TRUE)
description_df <- description_df[!is.na(description_df$text), , drop = FALSE]

# Create a corpus and remove stopwords
corpus_clean <- Corpus(VectorSource(description_df$text))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
# Normalize and lemmatize the text in the corpus
corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, content_transformer(lemmatize_strings))
inspect(corpus_clean[1:10])

description_df_clean <- data.frame(text = sapply(corpus_clean, as.character),
                                   stringsAsFactors = FALSE)

# Create dfm
desc_dfm <- description_df_clean$text |>
  quanteda::corpus() |>
  quanteda::tokens(remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) |>
  quanteda::tokens_remove(get_stopwords(language = "en")) |>
  quanteda::tokens_remove(pattern = "https?://\\S+|www\\.\\S+") |>
  quanteda::tokens_ngrams(n = 1:2, concatenator = " ") |>
  quanteda::dfm()

print(desc_dfm)

# Convert into dtm
desc_dtm <- convert(desc_dfm, to="topicmodels")
set.seed(7)

# LDA
lda_desc <- LDA(desc_dtm, method="Gibbs", k=8, control = list(alpha=0.1))
terms(lda_desc, 10) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"),  position = "left")

# LDAvis
desc_dtm = desc_dtm[slam::row_sums(desc_dtm) > 0, ]
phi = as.matrix(posterior(lda_desc)$terms)
theta <- as.matrix(posterior(lda_desc)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(desc_dtm)
term.freq = slam::col_sums(desc_dtm)[match(vocab, colnames(desc_dtm))]
json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)


# Find the most frequent job titles ---------------------------------------

# Preprocess the title data
# Filter out NA values in the "title" column
title_data <- exp %>%
  filter(!is.na(title))

# Preprocess the title data for bi-grams
bigrams <- title_data %>%
  select(title) %>%
  mutate(title = tolower(title)) %>%
  unnest_tokens(input = title, output = "tokens", token = "ngrams", n = 2) %>%
  mutate(tokens = str_replace_all(tokens, "[^[:alnum:]\\s]", ""))

trigrams <- title_data %>%
  select(title) %>%
  mutate(title = tolower(title)) %>%
  unnest_tokens(input = title, output = "tokens", token = "ngrams", n = 3) %>%
  mutate(tokens = str_replace_all(tokens, "[^[:alnum:]\\s]", ""))

# Load stopwords and additional custom words to remove
stop_words <- bind_rows(list(data.frame(word = stopwords("en"), stringsAsFactors = FALSE),
                             data.frame(word = c("title"), stringsAsFactors = FALSE)))

# Remove stop words
bigram_processed <- bigrams %>%
  anti_join(stop_words, by = c("tokens" = "word"))
trigram_processed <- trigrams %>%
  anti_join(stop_words, by = c("tokens" = "word"))

# Find the most frequent titles
top_bigram_titles <- bigram_processed %>%
  count(tokens, sort = TRUE) %>%
  top_n(40)

top_trigram_titles <- trigram_processed %>%
  count(tokens, sort = TRUE) %>%
  top_n(40)

# Print the most frequent titles
print(top_bigram_titles)
print(top_trigram_titles)

# Topic modelling on job skills ---------------------------------------

# Remove symbol and NA lines
skill <-  skill %>% 
  mutate(skills = gsub("[^A-Za-z0-9 ]", "", skills)) %>% 
  filter (!is.na(skill$skills))

# Create a corpus and remove stopwords
corpus_clean <- Corpus(VectorSource(skill$skills))
stopwords_to_remove <- c(stopwords("en"), stopwords("nl"))
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords_to_remove)
# Normalize and lemmatize the text in the corpus
corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))
inspect(corpus_clean[1:10])

skill_clean <- data.frame(text = sapply(corpus_clean, as.character),
                                   stringsAsFactors = FALSE)

# Create dfm
skill_dfm <- skill_clean$text |>
  quanteda::corpus() |>
  quanteda::tokens(remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) |>
  quanteda::tokens_remove(get_stopwords(language = "en")) |>
  quanteda::tokens_remove(pattern = "https?://\\S+|www\\.\\S+") |>
  quanteda::tokens_ngrams(n = 1:2, concatenator = " ") |>
  quanteda::dfm()

# Convert into dtm
skill_dtm <- convert(skill_dfm, to="topicmodels")
set.seed(7)

# LDA
lda_skill <- LDA(skill_dtm, method="Gibbs", k=6, control = list(alpha=0.1))
terms(lda_skill, 20) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"),  position = "left")

# LDAvis
skill_dtm = skill_dtm[slam::row_sums(skill_dtm) > 0, ]
phi = as.matrix(posterior(lda_skill)$terms)
theta <- as.matrix(posterior(lda_skill)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(skill_dtm)
term.freq = slam::col_sums(skill_dtm)[match(vocab, colnames(skill_dtm))]
json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)


# Get the topic distributions for each line in skill_clean
skill_topic_distributions <- as.data.frame(topics(lda_skill))
skill_df <- cbind(skill, skill_topic_distributions)


# Language ----------------------------------------------------------------
lang = read_csv('language_data.csv')
# Recode value
lang <- lang %>%
  mutate(language = case_when(
    language == "Engels" ~ "English",
    language == "Englisch" ~ "English",
    language == "Nederlands" ~ "Dutch",
    language == "Duits" ~ "German",
    language == "Frans" ~ "French",
    language == "Deutsch" ~ "German",
    TRUE ~ language
  ))

language_counts <- lang %>%
  group_by(language) %>%
  summarize(frequency = n()) %>% 
  arrange(desc(frequency))
print(language_counts)

lang <- lang %>%
  mutate(proficiency = ifelse(proficiency %in% c("FULL_PROFESSIONAL", "NATIVE_OR_BILINGUAL"), 1, 0))

lang_df <- lang %>%
  pivot_wider(names_from = language, values_from = proficiency) %>% 
  select(employee_id, English, Dutch, German, Spanish, French) %>% 
  mutate(English = ifelse(English == "NULL", 0, English),
         French = ifelse(French == "NULL", 0, French),
         German = ifelse(German == "NULL", 0, German),
         Dutch = ifelse(Dutch == "NULL", 0, Dutch),
         Spanish = ifelse(Spanish == "NULL", 0, Spanish))


# Education ---------------------------------------------------------------

edu = read_csv("education_data.csv")
  
# Find the highest education level of each person 
edu <- edu %>% 
  filter(is.na(end_year) | end_year < 2023) %>% 
  mutate(clean_degree = gsub("[^A-Za-z0-9]", "", degreeName)) %>% 
  mutate(clean_degree = case_when(
    grepl("BA|BS|BE|BICT", clean_degree) ~ "bachelor",
    grepl("bachelor|bsc|btech|bcom|bcs|beng|engineer|ingenieur|undergraduate|graduate|graduaat|bacharelado", tolower(clean_degree)) ~ "bachelor",
    grepl("MA|MS", clean_degree) ~ "master",
    grepl("msc|master|mba|mphil|postgrad|licentiate", tolower(clean_degree)) & !grepl("premaster", tolower(clean_degree)) ~ "master",
    grepl("doctor|postdoc|phd|dr", tolower(clean_degree))   & !grepl("medicaldoctor", tolower(clean_degree)) ~ "phd",
    grepl("exchange", tolower(clean_degree)) ~ NA_character_,
    TRUE ~ NA_character_
  )) %>% 
  mutate(degree_from_fieldofstudy = gsub("[^A-Za-z0-9 ]", "", fieldOfStudy)) %>% 
  mutate(degree_from_fieldofstudy = case_when(
    grepl("BA|BS|BE|BICT", clean_degree) ~ "bachelor",
    grepl("bsc|bachelor|btech|bcom|bcs|beng|undergraduate|graduate|graduaat|bacharelado", tolower(degree_from_fieldofstudy)) ~ "bachelor",
    grepl("MA|MS", degree_from_fieldofstudy) ~ "master",
    grepl("master|mba|msc|mphil|postgrad", tolower(degree_from_fieldofstudy)) & !grepl("premaster", tolower(degree_from_fieldofstudy)) ~ "master",
    grepl("doctor|postdoc|phd|dr", tolower(degree_from_fieldofstudy)) & !grepl("medicaldoctor", tolower(degree_from_fieldofstudy)) ~ "phd",
    grepl("exchange", clean_degree) ~ NA_character_,
    TRUE ~ NA_character_
  )) %>% 
  mutate(clean_degree = coalesce(clean_degree, degree_from_fieldofstudy))

  
degree_processed <- edu %>%
  group_by(employee_id) %>%
  summarise(highest_edu = case_when(
    "phd" %in% clean_degree ~ "phd",
    "master" %in% clean_degree ~ "master",
    "bachelor" %in% clean_degree ~ "bachelor",
    TRUE ~ NA_character_
  ))


sum(is.na(degree_processed$highest_edu))
sum(degree_processed$highest_edu == "bachelor", na.rm=TRUE)
sum(degree_processed$highest_edu == "master", na.rm=TRUE)
sum(degree_processed$highest_edu == "phd", na.rm=TRUE)


# Find the people who put PhD qualification in their experience 
# Filter to get those jobs with end_date smaller than current_date
exp_phd <- exp %>%
  mutate(end_year = year(as.Date(end_date))) %>% 
  filter(end_year < 2023) %>% 
  mutate(title = gsub("[^A-Za-z0-9 ]", "", tolower(title))) %>% 
  mutate(clean_degree = case_when(
    grepl("doctor|postdoc|phd|dr", title) ~ "phd",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(clean_degree)) %>% 
  select(employee_id, clean_degree, end_year)

# Mutate the column to change highest edu for those who had PhD position
degree_processed <- degree_processed %>%
  mutate(highest_edu = case_when(
    employee_id %in% exp_phd$employee_id[exp_phd$clean_degree == "phd"] ~ "phd",
    TRUE ~ highest_edu
  ))
# Number of PhD
sum(degree_processed$highest_edu == "phd", na.rm=TRUE)

# Find last year of education for each person
last_year <- edu %>%
  group_by(employee_id) %>%
  mutate(
    last_edu_year = max(end_year, na.rm = TRUE)
  ) %>%
  summarize(last_edu_year = max(last_edu_year)) %>% 
  mutate(
    last_edu_year = replace(last_edu_year, last_edu_year == -Inf, -1)
  )


# Filter people whose field of study is relevant to the data and software field
relevant_fields <- c("AI|IT|ICT",
                      "math|machine learning|ml|wiskunde|stat|web|informatic|comput|quantitative|informatik|informatica|",
                      "data|analytics|artificial intelligence|nlp|natural language processing|software|actuarial|actuary|deep learning|reinforcement learning",
                      "business intelligence|business engineer|programming|system|robot|information technology|information management")


# Keep 1 line for each person, retain his/her highest education and latest education year
abridged_edu <- edu %>%
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
         ))  %>% 
  group_by(employee_id) %>%
  mutate(
    has_relevant_field = as.integer(any(clean_field == 1 | field_from_degree == 1))
  ) %>% 
  summarize(has_relevant_field = max(has_relevant_field)) %>% 
  left_join(degree_processed, by = "employee_id") %>% 
  left_join(last_year, by = "employee_id")

table(abridged_edu$has_relevant_field)

# Keep the full history of education for each person, excluding education where
# the person graduates after 2023
processed_edu <- edu %>%
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
         ))  %>% 
  mutate(
    has_relevant_field = pmax(clean_field, field_from_degree)
  ) %>% 
  select(employee_id, has_relevant_field, clean_degree, end_year) %>% 
  bind_rows(exp_phd)




