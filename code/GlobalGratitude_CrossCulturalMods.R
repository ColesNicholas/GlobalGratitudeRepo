library(countrycode)
library(tidyverse)
library(here)

# specify directory
i_am("code/Cleaned_CrossCulturalMods.R")

#Main data country_code
DF_main <- readRDS(file = here("data", "GlobalGratitude_Final_Cleaned.Rds"))
DF_main <- DF_main %>%
  mutate(country_code = str_sub(lab, 1, 3)) %>%
  filter(!is.na(country_code))

#GDP
#Fetch main survey data
DF_GDP <- read.csv(file = here("data", "GlobalGratitude_GDPpc.csv"))
DF_GDP <- DF_GDP %>%
  rename("country_code" = "Country.Code") %>% 
  select(country_code,X2011:X2020)

DF_GDP <- DF_GDP %>%
  filter(country_code %in% unique(DF_main$country_code)) %>%
  mutate(mean_GDP = rowMeans(select(., X2011:X2020), na.rm = TRUE)) %>%
  select(country_code, mean_GDP)

#Relational Mobility
DF_main <- DF_main %>%
  mutate(
    rel_mob_mean = rowMeans(
      cbind(
        relational_mobility_1,
        relational_mobility_2,
        relational_mobility_3,
        7 - relational_mobility_4,
        7 - relational_mobility_5,
        relational_mobility_6,
        7 - relational_mobility_7,
        relational_mobility_8,
        7 - relational_mobility_9,
        relational_mobility_10,
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
        8 - responsibilism_4,
        8 - responsibilism_5
      ),
      na.rm = TRUE
    )
  )

DF_cultural <- DF_main %>%
  select(country_code, rel_mob_mean, respon_mean) %>%
  group_by(country_code) %>%
  summarise(
    rel_mob_mean = mean(rel_mob_mean, na.rm = TRUE),
    respon_mean = mean(respon_mean, na.rm = TRUE)
  )

#Tightness Looseness
DF_tight <- read.csv(file = here("data", "GlobalGratitude_Tightness.csv")) %>%
  rename(country = "ï..Country") %>%
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  filter(country_code %in% unique(DF_main$country_code)) %>% 
  select(Tightness:country_code)

#Hofstede
DF_indcol <- read.csv(file = here("data", "GlobalGratitude_Hofstede_ResMobility.csv"))
DF_indcol <- DF_indcol %>%
  mutate(country_code = countrycode(Matched, origin = 'country.name', destination = 'iso3c')) %>%
  filter(country_code %in% unique(DF_main$country_code)) %>%
  select(resmobility:country_code)

#Religiosity
DF_relig <- read.csv(file = here("data", "GlobalGratitude_WorldReligiosity.csv")) %>%
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  filter(country_code %in% unique(DF_main$country_code)) %>% 
  select(X2011:X2014,country_code) %>% 
  mutate(
    relig_mean = rowMeans(
      cbind(
        X2011,
        X2012,
        X2013,
        X2014),
      na.rm = TRUE)) %>% 
  select(country_code, relig_mean)

#Combine datasets

DF_combined <- DF_cultural %>%
  left_join(DF_tight, by = "country_code") %>%
  left_join(DF_relig, by = "country_code") %>%
  left_join(DF_GDP, by = "country_code") %>%
  left_join(DF_indcol, by = "country_code")

write.csv(DF_combined, 
          file = here('data',
                      "GlobalGratitude_CrossCulturalMod.csv"))

