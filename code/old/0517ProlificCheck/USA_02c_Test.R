# Dangs API credential at UCSD
qualtrics_api_credentials(
  api_key = 'urh7PtWEEOkAzrbjmYQC2FoipCvCT4n8ZIByL5rH', 
  base_url = 'iad1.qualtrics.com',
  install = TRUE,
  overwrite = TRUE)

readRenviron("~/.Renviron")

# open data
data <- fetch_survey(surveyID = 'SV_bdsDfqTOoqBEXno',
                   force_request = T,
                   label = F,
                   convert = F) %>% 
  filter(Finished != "0", condition !="NA")

data$gratitude_all <- rowMeans(data[, c("grateful", "thankful", "appreciative")], na.rm = TRUE)

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

# Create a list of datasets to compare
grat_datasets <- list(
  "Gratitude List" = grat_list$gratitude_all,
  "Gratitude Letter" = grat_letter$gratitude_all,
  "Gratitude Text" = grat_text$gratitude_all,
  "Chan Gratitude List" = grat_hk$gratitude_all,
  "Mental Subtraction Task" = grat_sing$gratitude_all, 
  "Gratitude to God Letter" = grat_god_letter$gratitude_all
)

# Create an empty dataframe to store results
results_df <- data.frame(Test = character(),
                         P_Value = numeric(),
                         Estimate = numeric(),
                         Statistic = numeric(),
                         Parameter = numeric(),
                         stringsAsFactors = FALSE)

# Loop through each dataset and perform the comparison
for (i in seq_along(grat_datasets)) {
  # Perform t-test
  t_test_result <- t.test(control$gratitude_all, grat_datasets[[i]])
  # Calculate effect size
  effect_size <- effsize::cohen.d(control$gratitude_all, grat_datasets[[i]], hedges.correction = F)
  
  # Add results to dataframe
  results_df <- rbind(results_df, data.frame(
    Test = paste("Measurement Only vs.", names(grat_datasets)[i]),
    Statistic = t_test_result$statistic,
    Parameter = t_test_result$parameter,
    P_Value = t_test_result$p.value,
    Estimate = effect_size$estimate
  ))
}

# Display dataframe
print(results_df)

###
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

TM <- read.csv("USA_002c_TaskMaster.csv") 
TM <- TM %>% filter(Finished != "0", condition !="NA")

demand <- TM %>%
  filter(!is.na(prq_g_1))

empathy <- TM %>%
  filter(!is.na(empathy_email_9))

kinship <- TM %>%
  filter(!is.na(sibling_gender))

TM$gratitude_module <- rowSums(TM[, c("Page_2_TimeOnPage", "Page_3_TimeOnPage", "Page_4_TimeOnPage",
                                          "Page_5_TimeOnPage", "Page_6_TimeOnPage", "Page_7_TimeOnPage",
                                          "Page_8_TimeOnPage", "Page_9_TimeOnPage", "Page_10_TimeOnPage",
                                          "Page_11_TimeOnPage", "Page_12_TimeOnPage", "Page_13_TimeOnPage",
                                          "Page_14_TimeOnPage", "Page_15_TimeOnPage", "Page_16_TimeOnPage",
                                          "Page_17_TimeOnPage", "Page_18_TimeOnPage", "Page_19_TimeOnPage",
                                          "Page_20_TimeOnPage", "Page_21_TimeOnPage", "Page_22_TimeOnPage",
                                          "Page_23_TimeOnPage", "Page_24_TimeOnPage", "Page_25_TimeOnPage",
                                          "Page_26_TimeOnPage", "Page_27_TimeOnPage", "Page_28_TimeOnPage")]) / 60

###
TM$shige_module <- rowSums(TM[, c("Page_29_TimeOnPage", "Page_30_TimeOnPage", "Page_31_TimeOnPage")]) / 60

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
kinship$deb_module <- rowSums(kinship[, c("Page_32_TimeOnPage", "Page_33_TimeOnPage", "Page_34_TimeOnPage",
                                             "Page_35_TimeOnPage", "Page_36_TimeOnPage", "Page_37_TimeOnPage",
                                             "Page_38_TimeOnPage", "Page_39_TimeOnPage", "Page_40_TimeOnPage")]) / 60
###

# Calculate mean values
mean_row <- data.frame(Name = "Mean",
                       Gratitude_Module = mean(TM$gratitude_module, na.rm = TRUE),
                       Shige_Module = mean(TM$shige_module, na.rm = TRUE),
                       Thomas_Module = mean(empathy$thomas_module, na.rm = TRUE),
                       Nicholas_Module = mean(demand$nicholas_module, na.rm = TRUE),
                       Deb_Module = mean(kinship$deb_module, na.rm = TRUE))

# Calculate median values
median_row <- data.frame(Name = "Median",
                         Gratitude_Module = median(TM$gratitude_module, na.rm = TRUE),
                         Shige_Module = median(TM$shige_module, na.rm = TRUE),
                         Thomas_Module = median(empathy$thomas_module, na.rm = TRUE),
                         Nicholas_Module = median(demand$nicholas_module, na.rm = TRUE),
                         Deb_Module = median(kinship$deb_module, na.rm = TRUE))

# Combine mean and median rows
module_time <- rbind(mean_row, median_row)
print(module_time)

view(TM$feedback)