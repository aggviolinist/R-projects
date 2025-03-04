library(tidyverse)
library(writexl)
library(readxl)

# Load the data
task_data <- read_excel("C:/Users/mulandi/Desktop/Take home Assignment/task_data.xlsx")

# Remove rows where Condition is "control"
task_data <- task_data %>%
  filter(Condition != "control")

# Convert text responses to numerical values
convert_response <- function(x) {
  case_when(
    x == "Not at all" ~ 0,
    grepl("Several days", x) ~ 1,
    grepl("Over half the days", x) ~ 2,
    grepl("Nearly/almost every day", x) ~ 3,
    TRUE ~ NA_real_  
  )
}

# Identify PHQ and GAD column names
phq_cols <- "^\\d+\\..*\\?$"
gad_cols <- "^\\d+\\..*\\?$"

# Selecting PHQ and GAD columns
phq_columns <- grep(phq_cols, names(task_data), value = TRUE)[1:8]
gad_columns <- grep(gad_cols, names(task_data), value = TRUE)[9:15]

task_data <- task_data %>%
  mutate(across(all_of(phq_columns), convert_response),
         across(all_of(gad_columns), convert_response))

# Rename columns for easier handling
new_phq_names <- paste0("PHQ_", 1:length(phq_columns))
new_gad_names <- paste0("GAD_", 1:length(gad_columns))

# Rename the columns
task_data <- task_data %>%
  rename(
    !!!setNames(phq_columns, new_phq_names),
    !!!setNames(gad_columns, new_gad_names)
  )

# Calculate total PHQ and GAD scores
task_data <- task_data %>%
  mutate(
    PHQ_Total = pmin(rowSums(select(., starts_with("PHQ_")), na.rm = TRUE), 24),
    GAD_Total = pmin(rowSums(select(., starts_with("GAD_")), na.rm = TRUE), 21)
  )

# Calculate the mean age
mean_age <- mean(task_data$Age, na.rm = TRUE)
task_data <- task_data %>%
  mutate(Mean_Age = mean_age)

#Sorting female vs males
num_females <- sum(task_data$Gender == "F", na.rm = TRUE)
num_males <- sum(task_data$Gender == "M", na.rm = TRUE)

cat("Number of Females:", num_females, "\n")
cat("Number of Males:", num_males, "\n")

# Cutoffs for depression severity
task_data <- task_data %>%
  mutate(
    PHQ_Severity = case_when(
      PHQ_Total < 5 ~ "None",
      PHQ_Total >= 5 & PHQ_Total < 10 ~ "Mild",
      PHQ_Total >= 10 & PHQ_Total < 15 ~ "Moderate",
      PHQ_Total >= 15 & PHQ_Total < 20 ~ "Moderately Severe",
      PHQ_Total >= 20 ~ "Severe",
      TRUE ~ NA_character_
    )
  )

# Cutoffs for anxiety severity
task_data <- task_data %>%
  mutate(
    GAD_Severity = case_when(
      GAD_Total < 5 ~ "None",
      GAD_Total >= 5 & GAD_Total < 10 ~ "Mild",
      GAD_Total >= 10 & GAD_Total < 15 ~ "Moderate",
      GAD_Total >= 15 ~ "Severe",
      TRUE ~ NA_character_
    )
  )

# Depression analysis by gender and form
depression_analysis <- task_data %>%
  group_by(Gender, Form, PHQ_Severity) %>%
  summarize(count = n(), .groups = 'drop')

print("Depression Analysis:")
print(depression_analysis, n = nrow(depression_analysis)) 

# Create a table of all the results
depression_analysis_table <- table(task_data$Gender, task_data$Form, task_data$PHQ_Severity)

# Print the table of all results
print("All Depression analysis results:")
print(depression_analysis_table)

# Effect of Shamiri Intervention on mental health
intervention_effect <- task_data %>%
  filter(Condition == "intervention") %>%
  group_by(Time) %>%
  summarise(
    mean_PHQ = mean(PHQ_Total, na.rm = TRUE),
    mean_GAD = mean(GAD_Total, na.rm = TRUE),
    .groups = "drop"
  )

# Print Intervention Effect
print("Intervention analysis from Baseline to Endpoint:")
print(intervention_effect)

write_xlsx(task_data, "clean_data.xlsx")
