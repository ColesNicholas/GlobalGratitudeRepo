library(tidyverse)
library(here)

## --------------------------------------------------------------------------------------
# specify directory
i_am("code/USA_02B_Harmonize.R")

## --------------------------------------------------------------------------------------
# Fetch USA_02b survey
data <- read.csv(file = here("data", 
                             "USA_02b_raw_unharmonized.csv"))

## --------------------------------------------------------------------------------------

# Change the condition column
data <- data %>% mutate(condition = case_when(
  condition == 1 ~ "measure",
  condition == 2 ~ "events",
  condition == 3 ~ "int.events",
  condition == 5 ~ "list",
  condition == 6 ~ "letter",
  condition == 7 ~ "text",
  condition == 8 ~ "hk.list",
  condition == 9 ~ "sub",
  condition == 11 ~ "god.letter",
  TRUE ~ as.character(condition)
))

# Change the condition_type column
data <- data %>% mutate(condition_type = case_when(
  condition_type == 1 ~ "control",
  condition_type == 2 ~ "intervention",
  TRUE ~ as.character(condition_type)
))

# Remove the color columns (results are inaccurate due to survey error)
data <- data %>% select(-color_task_red_6, -color_task_yellow_1, -color_task_blue_1)

# Save the processed data to CSV
write.csv(data, 
          file = here('data',
                      "USA_002b_raw_harmonized.csv")
          )
