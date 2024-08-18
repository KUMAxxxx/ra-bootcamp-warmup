# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(officer)
library(flextable)
library(writexl)

# Load the dataset
master <- readRDS("master.rds")

# *** Q1 - Count missing values for each variable
missing_counts <- master %>%
  summarise(across(everything(), ~sum(is.na(.)), .names = "missing_{.col}"))

missing_counts %>%
  distinct() %>%
  filter(row_number() == 1) %>%
  print()

# *** Q2 - Summary statistics for various subsets of data
summary_stats <- master %>%
  summarise(across(c(semester, graduate4yr, womengraduate4yr, mengraduate4yr, faculty, totcohortsize, instatetuition, costs), mean, na.rm = TRUE))

summary_stats_missing <- master %>%
  filter(is.na(yearofsem)) %>%
  summarise(across(c(semester, graduate4yr, womengraduate4yr, mengraduate4yr, faculty, totcohortsize, instatetuition, costs), mean, na.rm = TRUE))

summary_stats_non_missing <- master %>%
  filter(!is.na(yearofsem)) %>%
  summarise(across(c(semester, graduate4yr, womengraduate4yr, mengraduate4yr, faculty, totcohortsize, instatetuition, costs), mean, na.rm = TRUE))

# *** Q3, Q4 - Collapsing and plotting data
collapsed_data <- master %>%
  group_by(year) %>%
  summarise(semester = mean(semester, na.rm = TRUE),
            graduate4yr = mean(graduate4yr, na.rm = TRUE))

ggplot(collapsed_data, aes(x = year)) +
  geom_line(aes(y = semester), color = "black", size = 1) +
  geom_line(aes(y = graduate4yr), color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(name = "Fraction of schools on semester", limits = c(0.8, 1), 
                     sec.axis = sec_axis(~ ., name = "4-year graduation rate", limits = c(0.25, 0.45))) +
  labs(x = "Year") +
  theme_minimal() +
  ggtitle("Share on semesters vs Four year graduation rate") +
  theme(legend.position = "none") +
  ggsave("figure1.png", width = 8, height = 6)

# *** Q5 - Scatter plots
master <- master %>%
  mutate(per_women_cohort = w_cohortsize / totcohortsize,
         per_white_cohort = white_cohortsize / totcohortsize)

# Scatter plot for graduate4yr vs per_women_cohort
ggplot(master, aes(x = per_women_cohort, y = graduate4yr)) +
  geom_point(color = "blue", size = 1) +
  theme_minimal()

# Scatter plot for graduate4yr vs per_white_cohort
ggplot(master, aes(x = per_white_cohort, y = graduate4yr)) +
  geom_point(color = "blue", size = 1) +
  theme_minimal()

# Scatter plot for graduate4yr vs instatetuition
ggplot(master, aes(x = instatetuition, y = graduate4yr)) +
  geom_point(color = "blue", size = 1) +
  labs(x = "instatetuition") +
  theme_minimal()

# *** Part (b) - Replace missing 'after' with 0 and run regressions
master <- master %>%
  mutate(after = if_else(is.na(after), 0, after))

x_vars <- c("instatetuition", "costs", "faculty", "per_white_cohort")

# Regression 1: All
model_all <- feols(graduate4yr ~ after + instatetuition + costs + faculty + per_white_cohort | unitid + year, data = master, cluster = ~unitid)

# Regression 2: Women
model_women <- feols(womengraduate4yr ~ after + instatetuition + costs + faculty + per_white_cohort | unitid + year, data = master, cluster = ~unitid)

# Regression 3: Men
model_men <- feols(mengraduate4yr ~ after + instatetuition + costs + faculty + per_white_cohort | unitid + year, data = master, cluster = ~unitid)

# Prepare results for Excel export
results_table <- etable(model_all, model_women, model_men, se = TRUE, star = TRUE, cluster = TRUE)

# Export results to Excel
write_xlsx(results_table, path = "regression_results.xlsx")
