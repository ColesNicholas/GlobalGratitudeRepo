# specify directory
i_am("code/GlobalGratitude_Graphs.Rmd")

# fetch survey
DF <- readRDS(file = here("DF", "GlobalGratitude_Final_Cleaned.Rds"))

DF <- DF %>% 
  rowwise() %>% 
  mutate(
    # gratitude
    gratitude_mean = 
      mean(c(grateful, appreciative, thankful), 
           na.rm = T),
    
    # positive affect
    pa_mean = 
      mean(c(happy, satisfied, content, joyful, pleased), 
           na.rm = T),
    
    # optimism
    optimistic_mean = 
      mean(c(optimistic, hopeful), 
           na.rm = T),
    
    # negative affect
    na_mean = 
      mean(c(sad, depressed, anxious, nervous), 
           na.rm = T),
    
    # indebtendess
    indebted_mean = 
      mean(c(indebted, obligated), 
           na.rm = T),
    
    # envy
    envy_mean = 
      mean(c(envious, bitter, jealous), 
           na.rm = T),
    
    # life satisfaction
    ls_mean = 
      mean(c(ls_1, ls_2, ls_3, ls_4, ls_5), 
           na.rm = T),
    
    # sense of self
    ss_mean = 
      mean(c(self_image_circle, self_image), 
           na.rm = T)) %>% 
  ungroup()

# delete vestigial variables
DF <- DF %>%
  select(-c(grateful, appreciative, thankful, 
            happy, satisfied, content, 
            joyful, pleased, optimistic,
            hopeful,  depressed, sad,  anxious,  nervous,
            indebted, obligated,
            envious, bitter, jealous,
            ls_1, ls_2, ls_3,
            ls_4, ls_5, self_image_circle,
            self_image)
  )

DF <- DF %>% 
  filter(gratitude_mean != "NaN",
         pa_mean != "NaN",
         na_mean != "NaN",
         ls_mean != "NaN",
         optimistic_mean != "NaN",
         indebted_mean != "NaN",
         envy_mean != "NaN",
         guilty != "NaN",
         ss_mean != "NaN",
         ladder != "NaN")

measure <- subset(DF, condition == "measure")
events <- subset(DF, condition == "events")
int.events <- subset(DF, condition == "int.events")

list <- subset(DF, condition == "list")
letter <- subset(DF, condition == "letter")
text <- subset(DF, condition == "text")
hk.list <- subset(DF, condition == "hk.list")
sub <- subset(DF, condition == "sub")
god.letter <- subset(DF, condition == "god.letter")

mean_grat <- DF %>%
  group_by(condition) %>%
  summarise(grat_mean_value = mean(gratitude_mean, na.rm = TRUE))

mean_grat$grat_mean_difference <- mean_grat$grat_mean_value - 4.761997

mean_pa <- DF %>%
  group_by(condition) %>%
  summarise(pa_mean_value = mean(pa_mean, na.rm = TRUE))

mean_pa$pa_mean_difference <- mean_pa$pa_mean_value - 4.353087

mean_na <- DF %>%
  group_by(condition) %>%
  summarise(na_mean_value = mean(na_mean, na.rm = TRUE))

mean_na$na_mean_difference <- mean_na$na_mean_value - 2.976459

mean_ls <- DF %>%
  group_by(condition) %>%
  summarise(ls_mean_value = mean(ls_mean, na.rm = TRUE))

mean_ls$ls_mean_difference <- mean_ls$ls_mean_value - 4.169183

mean_optimistic <- DF %>%
  group_by(condition) %>%
  summarise(optimistic_mean_value = mean(optimistic_mean, na.rm = TRUE))

mean_optimistic$optimistic_mean_difference <- mean_optimistic$optimistic_mean_value - 4.566148

mean_indebted <- DF %>%
  group_by(condition) %>%
  summarise(indebted_mean_value = mean(indebted_mean, na.rm = TRUE))

mean_indebted$indebted_mean_difference <- mean_indebted$indebted_mean_value - 3.459438

mean_envy <- DF %>%
  group_by(condition) %>%
  summarise(envy_mean_value = mean(envy_mean, na.rm = TRUE))

mean_envy$envy_mean_difference <- mean_envy$envy_mean_value - 2.088266

mean_guilty <- DF %>%
  group_by(condition) %>%
  summarise(guilty_mean_value = mean(guilty, na.rm = TRUE))

mean_guilty$guilty_mean_difference <- mean_guilty$guilty_mean_value - 2.222915

mean_ss <- DF %>%
  group_by(condition) %>%
  summarise(ss_mean_value = mean(ss_mean, na.rm = TRUE))

mean_ss$ss_mean_difference <- mean_ss$ss_mean_value - 4.132193

mean_ladder <- DF %>%
  group_by(condition) %>%
  summarise(ladder_mean_value = mean(ladder, na.rm = TRUE))

mean_ladder$ladder_mean_difference <- mean_ladder$ladder_mean_value - 5.878694

mean_values <- bind_rows(
  mean_grat,
  mean_pa,
  mean_na,
  mean_ls,
  mean_optimistic,
  mean_indebted,
  mean_envy,
  mean_guilty,
  mean_ss,
  mean_ladder
)

grat_DFsets <- list(
  "list" = list$gratitude_mean,
  "letter" = letter$gratitude_mean,
  "text" = text$gratitude_mean,
  "hk.list" = hk.list$gratitude_mean,
  "sub" = sub$gratitude_mean,
  "god.letter" = god.letter$gratitude_mean
)
pa_DFsets <- list(
  "list" = list$pa_mean,
  "letter" = letter$pa_mean,
  "text" = text$pa_mean,
  "hk.list" = hk.list$pa_mean,
  "sub" = sub$pa_mean,
  "god.letter" = god.letter$pa_mean
)
na_DFsets <- list(
  "list" = list$na_mean,
  "letter" = letter$na_mean,
  "text" = text$na_mean,
  "hk.list" = hk.list$na_mean,
  "sub" = sub$na_mean,
  "god.letter" = god.letter$na_mean
)
ls_DFsets <- list(
  "list" = list$ls_mean,
  "letter" = letter$ls_mean,
  "text" = text$ls_mean,
  "hk.list" = hk.list$ls_mean,
  "sub" = sub$ls_mean,
  "god.letter" = god.letter$ls_mean
)

optimistic_DFsets <- list(
  "list" = list$optimistic_mean,
  "letter" = letter$optimistic_mean,
  "text" = text$optimistic_mean,
  "hk.list" = hk.list$optimistic_mean,
  "sub" = sub$optimistic_mean,
  "god.letter" = god.letter$optimistic_mean
)

indebted_DFsets <- list(
  "list" = list$indebted_mean,
  "letter" = letter$indebted_mean,
  "text" = text$indebted_mean,
  "hk.list" = hk.list$indebted_mean,
  "sub" = sub$indebted_mean,
  "god.letter" = god.letter$indebted_mean
)

envy_DFsets <- list(
  "list" = list$envy_mean,
  "letter" = letter$envy_mean,
  "text" = text$envy_mean,
  "hk.list" = hk.list$envy_mean,
  "sub" = sub$envy_mean,
  "god.letter" = god.letter$envy_mean
)

guilty_DFsets <- list(
  "list" = list$guilty,
  "letter" = letter$guilty,
  "text" = text$guilty,
  "hk.list" = hk.list$guilty,
  "sub" = sub$guilty,
  "god.letter" = god.letter$guilty
)

ss_DFsets <- list(
  "list" = list$ss_mean,
  "letter" = letter$ss_mean,
  "text" = text$ss_mean,
  "hk.list" = hk.list$ss_mean,
  "sub" = sub$ss_mean,
  "god.letter" = god.letter$ss_mean
)

ladder_DFsets <- list(
  "list" = list$ladder,
  "letter" = letter$ladder,
  "text" = text$ladder,
  "hk.list" = hk.list$ladder,
  "sub" = sub$ladder,
  "god.letter" = god.letter$ladder
)

results_df <- data.frame(condition = character(),
                         grat_effect_size = numeric(),
                         pa_effect_size = numeric(),
                         na_effect_size = numeric(),
                         ls_effect_size = numeric(),
                         optimistic_effect_size = numeric(),
                         indebted_effect_size = numeric(),
                         envy_effect_size = numeric(),
                         guilty_effect_size = numeric(),
                         ss_effect_size = numeric(),
                         ladder_effect_size = numeric(),
                         stringsAsFactors = FALSE)

# Loop through each DFset and perform the comparison
for (i in seq_along(grat_DFsets)) {
  # Calculate effect size for each measure
  grat_effect_size <- effsize::cohen.d(measure$gratitude_mean, grat_DFsets[[i]], hedges.correction = TRUE)
  pa_effect_size <- effsize::cohen.d(measure$pa_mean, pa_DFsets[[i]], hedges.correction = TRUE)
  na_effect_size <- effsize::cohen.d(measure$na_mean, na_DFsets[[i]], hedges.correction = TRUE)
  ls_effect_size <- effsize::cohen.d(measure$ls_mean, ls_DFsets[[i]], hedges.correction = TRUE)
  optimistic_effect_size <- effsize::cohen.d(measure$optimistic_mean, optimistic_DFsets[[i]], hedges.correction = TRUE)
  indebted_effect_size <- effsize::cohen.d(measure$indebted_mean, indebted_DFsets[[i]], hedges.correction = TRUE)
  envy_effect_size <- effsize::cohen.d(measure$envy_mean, envy_DFsets[[i]], hedges.correction = TRUE)
  guilty_effect_size <- effsize::cohen.d(measure$guilty, guilty_DFsets[[i]], hedges.correction = TRUE)
  ss_effect_size <- effsize::cohen.d(measure$ss_mean, ss_DFsets[[i]], hedges.correction = TRUE)
  ladder_effect_size <- effsize::cohen.d(measure$ladder, ladder_DFsets[[i]], hedges.correction = TRUE)

  # Store results in the DFframe
  results_df <- rbind(results_df, data.frame(
    condition = names(grat_DFsets)[i],
    grat_effect_size = grat_effect_size$estimate * -1,
    pa_effect_size = pa_effect_size$estimate * -1,
    na_effect_size = na_effect_size$estimate * -1,
    ls_effect_size = ls_effect_size$estimate * -1,
    optimistic_effect_size = optimistic_effect_size$estimate * -1,
    indebted_effect_size = indebted_effect_size$estimate * -1,
    envy_effect_size = envy_effect_size$estimate * -1,
    guilty_effect_size = ls_effect_size$estimate * -1,
    ss_effect_size = ss_effect_size$estimate * -1,
    ladder_effect_size = ladder_effect_size$estimate * -1
  ))
}

# Print the results to view the effect sizes
print(results_df)

mean_DF <- bind_rows(mean_values, results_df)

write.csv(mean_DF, 
        file = here('data',
                    "GlobalGratitude_Means.csv"))
