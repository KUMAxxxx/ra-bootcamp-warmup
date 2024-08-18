library(readr)
library(dplyr)
library(tidyr)
library(readxl)

# *** Semester dummy
semester_data_2 <- read_csv("semester_data_2.csv")
saveRDS(semester_data_2, "semester_data_2.rds")

semester_data_1 <- read_csv("semester_data_1.csv")
saveRDS(semester_data_1, "semester_data_1.rds")

semester_data <- bind_rows(semester_data_1, semester_data_2)

semester_data <- semester_data %>%
  arrange(unitid, year) %>%
  group_by(unitid) %>%
  mutate(pre_quarter = lag(quarter),
         spoint = if_else(!is.na(pre_quarter), pre_quarter - quarter, 0),
         filled_value = if_else(spoint == 1, year, NA_real_)) %>%
  ungroup() %>%
  group_by(unitid) %>%
  mutate(yearofsem = min(filled_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(after = if_else(!is.na(yearofsem) & year >= yearofsem, 1, 0)) %>%
  select(-pre_quarter, -spoint, -filled_value)

saveRDS(semester_data, "clean_semester_dummy.rds")

# *** Outcome
filelist <- list.files(pattern = "\\.xlsx$")

outcome_data <- filelist %>%
  lapply(read_excel, guess_max = 10000) %>%
  lapply(function(df) {
    df %>%
      mutate(totcohortsize = as.numeric(totcohortsize),
             m_4yrgrads = as.numeric(m_4yrgrads))
  }) %>%
  bind_rows()

outcome_data <- outcome_data %>%
  filter(!is.na(totcohortsize)) %>%
  arrange(year, unitid) %>%
  mutate(womengraduate4yr = women_gradrate_4yr * 0.01,
         graduate4yr = tot4yrgrads / totcohortsize,
         mengraduate4yr = m_4yrgrads / m_cohortsize)

outcome_data <- outcome_data %>%
  mutate(across(c(womengraduate4yr, graduate4yr, mengraduate4yr), round, 4))

saveRDS(outcome_data, "clean_outcome.rds")

# *** Covariates
covariates <- read_excel("covariates.xlsx")

covariates <- covariates %>%
  mutate(unitid = substr(university_id, 1, 6),
         unitid = as.numeric(unitid),
         year = as.numeric(year),
         value = as.numeric(value)) %>%
  select(-university_id) %>%
  pivot_wider(names_from = category, values_from = value) %>%
  rename(costs = `1`,
         faculty = `2`,
         instatetuition = `3`,
         white_cohortsize = `4`) %>%
  filter(year > 1990 & year < 2011) %>%
  filter(!is.na(costs) & !is.na(faculty) & !is.na(instatetuition) & !is.na(white_cohortsize)) %>%
  arrange(unitid, year)

saveRDS(covariates, "clean_covariates.rds")

# *** Master
semester_dummy <- readRDS("clean_semester_dummy.rds")
outcome_data <- readRDS("clean_outcome.rds")
covariates <- readRDS("clean_covariates.rds")

master_data <- semester_dummy %>%
  left_join(outcome_data, by = c("unitid", "year")) %>%
  left_join(covariates, by = c("unitid", "year")) %>%
  filter(!is.na(costs))

saveRDS(master_data, "master.rds")
