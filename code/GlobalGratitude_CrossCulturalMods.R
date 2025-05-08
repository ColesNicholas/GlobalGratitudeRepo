library(countrycode)
library(tidyverse)
library(here)

# specify directory
i_am("code/GlobalGratitude_CrossCulturalMods.R")

#Main data country_code
DF_main <- readRDS(file = here("data", "GlobalGratitude_Final_Cleaned.Rds"))
DF_main <- DF_main %>%
  mutate(
    country_code = str_sub(lab, 1, 3)) %>%
  filter(!is.na(country_code))

sampled <- unique(DF_main$country_code)

#Relational Mobility
DF_main <- DF_main %>%
  mutate(
    rel_mob_mean = rowMeans(
      cbind(
        relational_mobility_1,
        relational_mobility_2,
        relational_mobility_3,
        relational_mobility_4,
        relational_mobility_5,
        relational_mobility_6,
        7 - relational_mobility_7,
        7 - relational_mobility_8,
        7 - relational_mobility_9,
        7 - relational_mobility_10,
        7 - relational_mobility_11,
        7 - relational_mobility_12
      ),
      na.rm = TRUE
    )
  )

#Responsibilism
DF_main <- DF_main %>%
  mutate(
    respon_mean = rowMeans(
      cbind(
        responsibilism_1,
        responsibilism_2,
        responsibilism_3,
        responsibilism_4,
        8 - responsibilism_5
      ),
      na.rm = TRUE
    )
  )

DF_cultural <- DF_main %>%
  select(country_code, rel_mob_mean, respon_mean) %>% 
  mutate(country_name = countrycode(country_code, origin = "iso3c", destination = "country.name")) %>%
  group_by(country_code, country_name) %>%
  summarise(
    rel_mob_mean = mean(rel_mob_mean, na.rm = TRUE),  
    respon_mean = mean(respon_mean, na.rm = TRUE),
    .groups = "drop"
  )

#Relational Mobility
DF_relation <- read.csv(file = here("data", "GlobalGratitude_RelationalMobility.csv"))
DF_relation <- DF_relation %>%
  rename("country_name" = "COUNTRY") %>% 
  rename("relational_mobility" = "RMOBLM") %>% 
  mutate(country_code = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>% 
  select(country_code, country_name, relational_mobility)
  
#Responsibilism
DF_respon <- read.csv(file = here("data", "GlobalGratitude_Responsibilism.csv"))
DF_respon <- DF_respon %>%
  rename("country_name" = "ï..Culture") %>% 
  rename("responsibilism" = "ResponsibilismSum5Items") %>% 
  mutate(country_code = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>% 
  filter(country_code != "Overall Average") %>%
  mutate(country_name = ifelse(country_name == "UK", "United Kingdom", country_name)) 

#GDP
#Fetch main survey data
DF_GDP <- read.csv(file = here("data", "GlobalGratitude_GDPpc.csv"))
DF_GDP <- DF_GDP %>%
  rename("country_code" = "Country.Code") %>%
  select(country_code, X2011:X2020) %>%
  mutate(country_name = countrycode(country_code, origin = "iso3c", destination = "country.name")) %>% 
  mutate(mean_GDP = rowMeans(select(., X2011:X2020), na.rm = TRUE)) %>%
  select(country_code, country_name, mean_GDP)
DF_GDP <- DF_GDP %>%
  filter(country_name != "NA")

#Tightness Looseness
DF_tight <- read.csv(file = here("data", "GlobalGratitude_Tightness.csv")) %>%
  rename(country_name = ï..Country) %>%
  mutate(country_code = countrycode(country_name, origin = 'country.name', destination = 'iso3c')) %>% 
  select(tightness = Tightness, country_code,country_name)

#Hofstede
DF_indcol <- read.csv(file = here("data", "GlobalGratitude_Hofstede_ResMobility.csv"))
DF_indcol <- DF_indcol %>%
  mutate(country_code = countrycode(Matched, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(country_name = countrycode(country_code, origin = "iso3c", destination = "country.name")) %>%
  select(resmobility:country_name)

#Religiosity
DF_relig <- read.csv(file = here("data", "GlobalGratitude_WorldReligiosity.csv")) %>%
  rename("country_name" = "country") %>% 
  mutate(country_code = countrycode(country_name, origin = 'country.name', destination = 'iso3c')) %>%
  select(X2011:X2014, country_name, country_code) %>%
  mutate(
    relig_mean = rowMeans(
      cbind(X2011, X2012, X2013, X2014),
      na.rm = TRUE)) %>%
  select(country_code, country_name, relig_mean) %>%
  mutate(country_name = ifelse(country_name == "Macedonia", "North Macedonia", country_name)) 


#Combine datasets

DF_combined <- DF_cultural %>%
  full_join(DF_relation, by = c("country_name", "country_code")) %>%
  full_join(DF_respon, by = c("country_name", "country_code")) %>%
  full_join(DF_tight, by = c("country_name", "country_code")) %>%
  full_join(DF_relig, by = c("country_name", "country_code")) %>%
  full_join(DF_GDP, by = c("country_name", "country_code")) %>%
  full_join(DF_indcol, by = c("country_name", "country_code")) %>%
  filter(!is.na(country_name))

DF_combined$sample <- ifelse(DF_combined$country_code %in% sampled, "sampled", "not sampled") 
DF_combined <- DF_combined %>%
  mutate(country_name = ifelse(country_name == "North Macedonia", "Macedonia", country_name)) 

write.csv(DF_combined, 
          file = here('data',
                      "GlobalGratitude_CrossCulturalMod.csv"),
          row.names = F)

