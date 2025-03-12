library(here)
library(dplyr)

# specify directory
i_am("code/GlobalGratitude_Graphs.Rmd")

# fetch survey
DF <- readRDS(file = here("data", "GlobalGratitude_Final_Cleaned.Rds"))

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
    
    # negative affect (reversed scored)
    na_mean = 
      mean(c(-sad, -depressed, -anxious, -nervous), 
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

# Rescale ladder from 11-point scale (0 to 10) to 7-point scale (1 to 7)
rescale <- function(x) {
  new_min <- 1
  new_max <- 7
  old_min <- 0
  old_max <- 10
  return(((x - old_min) / (old_max - old_min)) * (new_max - new_min) + new_min)
}

DF$ladder_rescaled <- rescale(DF$ladder)


#Calculate measurement only means
measurement_only <- DF %>% 
  filter(condition == "measure") %>% 
  summarise(measurement_pa_mean = mean(pa_mean, na.rm = TRUE),
            measurement_na_mean = mean(na_mean, na.rm = TRUE),
            measurement_ls_mean= mean(ls_mean, na.rm = TRUE), 
            measurement_optimism_mean = mean(optimistic_mean, na.rm = TRUE),
            measurement_ladder_mean = mean(ladder_rescaled, na.rm = TRUE),
            measurement_indebtedness_mean = mean(indebted_mean, na.rm = TRUE))

#PA and NA (on 7-point scale)
mood_mean_values <- DF %>% group_by(condition) %>%
  summarise(
    pa_mean = mean(pa_mean, na.rm = TRUE), 
    pa_mean_diff = pa_mean - measurement_only$measurement_pa_mean, 
    na_mean = mean(na_mean, na.rm = TRUE),
    na_mean_diff = na_mean - measurement_only$measurement_na_mean,
    mood_mean_diff = mean(c(pa_mean_diff, na_mean_diff)))

#Life Satisfaction, Optimism, Attitude toward life (on 7-point scale)
satisfaction_mean_values <- DF %>%
  group_by(condition) %>%
  summarise(
    ls_mean = mean(ls_mean, na.rm = TRUE), 
    ls_mean_diff = ls_mean - measurement_only$measurement_ls_mean, 
    optimism_mean = mean(optimistic_mean, na.rm = TRUE),
    optimism_mean_diff = optimism_mean - measurement_only$measurement_optimism_mean,
    ladder_mean = mean(ladder_rescaled, na.rm = TRUE),
    ladder_mean_diff = ladder_mean - measurement_only$measurement_ladder_mean,
    satisfaction_mean_diff = mean(c(ls_mean_diff, optimism_mean_diff, ladder_mean_diff), na.rm = TRUE)  # Average of the three scales
  )

#Indebtedness (on a 7-point scale)
indebtedness_mean_values <- DF %>% group_by(condition) %>%
  summarise(
    indebtedness_mean = mean(indebted_mean, na.rm = TRUE),
    indebtedness_mean_diff = indebtedness_mean - measurement_only$measurement_indebtedness_mean)
  
#Combined Dataframes
mean_values <- mood_mean_values %>% 
  left_join(satisfaction_mean_values, by = "condition") %>% 
  left_join(indebtedness_mean_values, by = "condition")

#Write csv
write.csv(mean_values, 
          file = here('data',
                      "GlobalGratitude_AggregatedMeans.csv"))
