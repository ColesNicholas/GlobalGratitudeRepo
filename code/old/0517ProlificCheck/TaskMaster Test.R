#TaskMaster Testing

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

data <- read.csv("TaskMaster Test.csv")

data <- data %>%
  # remove incomplete or test observations
  filter(Finished != "0", condition !="NA")

demand <- data %>%
  filter(!is.na(prq_g_1))

empathy <- data %>%
  filter(!is.na(empathy_email_9))

control <- subset(data, condition == "measure")
control_events <- subset(data, condition == "events")
control_interesting <- subset(data, condition == "int.events")

grat_list <- subset(data, condition == "list")
grat_letter <- subset(data, condition == "letter")
grat_text <- subset(data, condition == "text")
grat_hk <- subset(data, condition == "hk.list")
grat_sing <- subset(data, condition == "sub")
grat_god_letter <- subset(data, condition == "god.letter")

###
writing_mean <- list(
  "MO" = control$Page_2_TimeOnPage,
  "Events" = control_events$Page_2_TimeOnPage,
  "Interesting Events" = control_interesting$Page_2_TimeOnPage,
  "Gratitude List" = grat_list$Page_2_TimeOnPage,
  "Gratitude Letter" = grat_letter$Page_2_TimeOnPage,
  "Gratitude Text" = grat_text$Page_2_TimeOnPage,
  "Chan Gratitude List" = grat_hk$Page_2_TimeOnPage,
  "Mental Subtraction Task" = grat_sing$Page_2_TimeOnPage, 
  "Gratitude to God Letter" = grat_god_letter$Page_2_TimeOnPage
)

results_df <- data.frame(Test = character(),
                         Mean = numeric(),
                         Median = numeric(),
                         stringsAsFactors = FALSE)

for (task_name in names(writing_mean)) {
  # Perform mean
  mean_value <- mean(writing_mean[[task_name]] / 60, na.rm = TRUE)
  # Perform median
  median_value <- median(writing_mean[[task_name]] / 60, na.rm = TRUE)
  
  # Add results to results_df
  results_df <- rbind(results_df, data.frame(Test = task_name, Mean = mean_value, Median = median_value))
}

print(results_df)

###

#Time on the gratitude module
data$gratitude_module <- rowSums(data[, c("Page_2_TimeOnPage", "Page_3_TimeOnPage", "Page_4_TimeOnPage",
                                          "Page_5_TimeOnPage", "Page_6_TimeOnPage", "Page_7_TimeOnPage",
                                          "Page_8_TimeOnPage", "Page_9_TimeOnPage", "Page_10_TimeOnPage",
                                          "Page_11_TimeOnPage", "Page_12_TimeOnPage", "Page_13_TimeOnPage",
                                          "Page_14_TimeOnPage", "Page_15_TimeOnPage", "Page_16_TimeOnPage",
                                          "Page_17_TimeOnPage", "Page_18_TimeOnPage", "Page_19_TimeOnPage",
                                          "Page_20_TimeOnPage", "Page_21_TimeOnPage", "Page_22_TimeOnPage",
                                          "Page_23_TimeOnPage", "Page_24_TimeOnPage", "Page_25_TimeOnPage",
                                          "Page_26_TimeOnPage", "Page_27_TimeOnPage", "Page_28_TimeOnPage")]) / 60

###
data$shige_module <- rowSums(data[, c("Page_29_TimeOnPage", "Page_30_TimeOnPage", "Page_31_TimeOnPage")]) / 60

###
empathy$thomas_module <- rowSums(empathy[, c("Page_32_TimeOnPage", "Page_33_TimeOnPage", "Page_34_TimeOnPage",
                                          "Page_35_TimeOnPage", "Page_36_TimeOnPage", "Page_37_TimeOnPage",
                                          "Page_38_TimeOnPage", "Page_39_TimeOnPage")]) / 60

###
demand$nicholas_module <- rowSums(demand[, c("Page_32_TimeOnPage", "Page_33_TimeOnPage", "Page_34_TimeOnPage",
                                             "Page_35_TimeOnPage", "Page_36_TimeOnPage", "Page_37_TimeOnPage",
                                             "Page_38_TimeOnPage", "Page_39_TimeOnPage", "Page_40_TimeOnPage",
                                           "Page_41_TimeOnPage", "Page_42_TimeOnPage", "Page_43_TimeOnPage",
                                           "Page_44_TimeOnPage")]) / 60

###

# Calculate mean values
mean_row <- data.frame(Name = "Mean",
                       Gratitude_Module = mean(data$gratitude_module, na.rm = TRUE),
                       Shige_Module = mean(data$shige_module, na.rm = TRUE),
                       Thomas_Module = mean(empathy$thomas_module, na.rm = TRUE),
                       Nicholas_Module = mean(demand$nicholas_module, na.rm = TRUE))

# Calculate median values
median_row <- data.frame(Name = "Median",
                         Gratitude_Module = median(data$gratitude_module, na.rm = TRUE),
                         Shige_Module = median(data$shige_module, na.rm = TRUE),
                         Thomas_Module = median(empathy$thomas_module, na.rm = TRUE),
                         Nicholas_Module = median(demand$nicholas_module, na.rm = TRUE))

# Combine mean and median rows
module_time <- rbind(mean_row, median_row)
print(module_time)