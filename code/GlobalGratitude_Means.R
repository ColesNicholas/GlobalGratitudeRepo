measure <- subset(data, condition == "measure")
events <- subset(data, condition == "events")
int.events <- subset(data, condition == "int.events")

list <- subset(data, condition == "list")
letter <- subset(data, condition == "letter")
text <- subset(data, condition == "text")
hk.list <- subset(data, condition == "hk.list")
sub <- subset(data, condition == "sub")
god.letter <- subset(data, condition == "god.letter")

mean_grat <- data %>%
  group_by(condition) %>%
  summarise(grat_mean_value = mean(gratitude_all))

mean_grat$grat_mean_difference <- mean_grat$grat_mean_value - 4.761997

mean_pa <- data %>%
  group_by(condition) %>%
  summarise(pa_mean_value = mean(happy_all))

mean_pa$pa_mean_difference <- mean_pa$pa_mean_value - 4.353087

mean_na <- data %>%
  group_by(condition) %>%
  summarise(na_mean_value = mean(sad_all))

mean_na$na_mean_difference <- mean_na$na_mean_value - 2.976459

mean_ls <- data %>%
  group_by(condition) %>%
  summarise(ls_mean_value = mean(ls_all))

mean_ls$ls_mean_difference <- mean_ls$ls_mean_value - 4.169183

# Create a list of datasets to compare
grat_datasets <- list(
  "list" = list$gratitude_all,
  "letter" = letter$gratitude_all,
  "text" = text$gratitude_all,
  "hk.list" = hk.list$gratitude_all,
  "sub" = sub$gratitude_all,
  "god.letter" = god.letter$gratitude_all
)
pa_datasets <- list(
  "list" = list$happy_all,
  "letter" = letter$happy_all,
  "text" = text$happy_all,
  "hk.list" = hk.list$happy_all,
  "sub" = sub$happy_all,
  "god.letter" = god.letter$happy_all
)
na_datasets <- list(
  "list" = list$sad_all,
  "letter" = letter$sad_all,
  "text" = text$sad_all,
  "hk.list" = hk.list$sad_all,
  "sub" = sub$sad_all,
  "god.letter" = god.letter$sad_all
)
ls_datasets <- list(
  "list" = list$ls_all,
  "letter" = letter$ls_all,
  "text" = text$ls_all,
  "hk.list" = hk.list$ls_all,
  "sub" = sub$ls_all,
  "god.letter" = god.letter$ls_all
)

# Create an empty dataframe to store results
results_df <- data.frame(condition = character(),
                         grat_effect_size = numeric(),
                         pa_effect_size = numeric(),
                         na_effect_size = numeric(),
                         ls_effect_size = numeric(),
                         stringsAsFactors = FALSE)

# Loop through each dataset and perform the comparison
for (i in seq_along(grat_datasets)) {
  # Calculate effect size
  grat_effect_size <- effsize::cohen.d(measure$gratitude_all, grat_datasets[[i]], hedges.correction = TRUE)
  pa_effect_size <- effsize::cohen.d(measure$happy_all, pa_datasets[[i]], hedges.correction = TRUE)
  na_effect_size <- effsize::cohen.d(measure$sad_all, na_datasets[[i]], hedges.correction = TRUE)
  ls_effect_size <- effsize::cohen.d(measure$ls_all, ls_datasets[[i]], hedges.correction = TRUE)
  
  # Add results to dataframe
  results_df <- rbind(results_df, data.frame(
    Test = (names(grat_datasets)[i]),
    grat_effect_size = grat_effect_size$estimate*-1,
    pa_effect_size = pa_effect_size$estimate*-1,
    na_effect_size = na_effect_size$estimate*-1,
    ls_effect_size = ls_effect_size$estimate*-1 
  ))
}

# Display dataframe
print(results_df)


mean_data <- bind_rows(mean_grat, mean_pa, mean_na, mean_ls, results_df)

write.csv(mean_data, 
        file = here('data',
                    "GlobalGratitude_Means.csv"))
