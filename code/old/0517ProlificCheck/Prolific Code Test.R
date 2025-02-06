library(qualtRics)
library(tidyverse)
library(dplyr)

# Dangs API credential at UCSD
qualtrics_api_credentials(
  api_key = 'urh7PtWEEOkAzrbjmYQC2FoipCvCT4n8ZIByL5rH', 
  base_url = 'iad1.qualtrics.com',
  install = TRUE,
  overwrite = TRUE)

readRenviron("~/.Renviron")

# open data
DF <- fetch_survey(surveyID = 'SV_0HYWYhgl3wM8F7g',
                   force_request = T,
                   label = F,
                   convert = F)

# process data
data <- DF %>%
  # remove incomplete or test observations
  filter(Finished != "0", condition !="NA")

data$gratitude_all <- rowMeans(data[, c("grateful", "thankful", "appreciative")], na.rm = TRUE)

#Conditions

control <- subset(data, condition == "1")
control_events <- subset(data, condition == "2")
control_interesting <- subset(data, condition == "3")

grat_list <- subset(data, condition == "5")
grat_letter <- subset(data, condition == "6")
grat_text <- subset(data, condition == "7")
grat_hk <- subset(data, condition == "8")
grat_sing <- subset(data, condition == "9")
grat_god_letter <- subset(data, condition == "11")

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

write.csv(results_df, file = "AD.tmp.csv")

god <- effsize::cohen.d(control$gratitude_all, grat_god_letter$gratitude_all, hedges.correction = F)
print(god)
