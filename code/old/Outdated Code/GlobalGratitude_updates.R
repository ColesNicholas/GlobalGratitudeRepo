## --------------------------------------------------------------------------------------
# clear environment
rm(list = ls())

# install (if necessary) and load packages
  # function written by stevenworthington 
  Ipak <- function(pkg){
      new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
      
      if (length(new.pkg)) 
          install.packages(new.pkg, dependencies = TRUE)
      
      sapply(pkg, require, character.only = TRUE)
  }

  # vector of necessary packages
  packages <- c('tidyverse', 'ggplot2', 
                'qualtRics', 'googlesheets4')
  
  # using vector of packages, call ipak function
  Ipak(packages)
  
  # delete vestigial
  rm(packages, Ipak)


## --------------------------------------------------------------------------------------
# Dangs API credential at UCSD
qualtrics_api_credentials(
  api_key = 'urh7PtWEEOkAzrbjmYQC2FoipCvCT4n8ZIByL5rH', 
  base_url = 'iad1.qualtrics.com',
  install = TRUE,
  overwrite = TRUE)

readRenviron("~/.Renviron")

## --------------------------------------------------------------------------------------
# fetch survey
DF <- fetch_survey(surveyID = 'SV_1YQ3FmlbEHp1RIO',
                   force_request = T,
                   label = F,
                   convert = F)

# fix known issues
DF <- DF %>% 
  
  # 4/22/2024 TUR_01 used real link for testing purposes
  filter(ResponseId != "R_42KUGZSS76NgWH7",
         ResponseId != "R_4W4EXfgeyk1rCYF",
         ResponseId != "R_45Z862EIfzYcin4",
         ResponseId != "R_7BhJx9Ci7THupmF",
         ResponseId != "R_42Lv9fg5qi9V9xm",
         ResponseId != "R_2kFa26mh78uevnz",
         ResponseId != "R_4DRThnH8LgbEvM8",
         ResponseId != "R_4r1kAEqQCo1PNn6",
         ResponseId != "R_4GQErqyYDlRWVwZ",
         ResponseId != "R_4SGF0GCHSHIN0ls",
         ResponseId != "R_6Pndj5c7sr1pcU3",
         ResponseId != "R_8lQxsY2ITg7HKh1",
         #USA_01 duplicate data
         ResponseId != "R_6rDfD5u84z6WufT",
         #DZA_01 test data
         ResponseId != "R_4ioYJK1zR2FgR4R",
         ResponseId != "R_4OvlyOmeTsmLpFn",
         ResponseId != "R_4BA1gbglSYnVyDK",
        #TUR_01 duplicate data
        ResponseId != "R_8HXsI5PQftiJUYk",
        ResponseId != "R_8iVWI3CN49ACiUp",
        #Remove BeSample
        lab != "KEN_01",
        #MKD_01 test data
        ResponseId != "R_2MfI2eD3t5GDdV7",
        ResponseId != "R_8IY7WUpE9jcRxw4",
        ResponseId != "R_8bDAs5tKlqK1X7r",
        ResponseId != "R_2FDhMB2txdwRiG1",
        ResponseId != "R_2qkChj4zKoDZsrf")

 
# calculate n total
n.total <- DF %>%
  filter(DistributionChannel != "preview") %>% 
  group_by(lab) %>% 
  summarise(`n (total)` = n())

# calculate n consenting
n.consent <- DF %>%
  filter(DistributionChannel != "preview",
         consent == 1) %>% 
  group_by(lab) %>%
  summarise(`n (consenting)` = n())

# calculate summary statistics for consenting complete participants
n.complete <- DF %>%
  filter(DistributionChannel != "preview",
         consent == 1,
         Progress > 95) %>% 
  group_by(lab) %>%
  summarise(`n (consenting and complete)` = n(),
            `mean duration (mins)` = 
              mean(`Duration (in seconds)`) / 60,
            `median duration (mins)` = 
              median(`Duration (in seconds)`) / 60)

# combine summaries
summary <- 
  full_join(x = n.total,
            y = n.consent) %>% 
  full_join(x = .,
            y = n.complete) %>% 
  
  # remove NA's 
  #filter(!is.na(lab)) %>% 
  
  # round duration statistics
  mutate(`mean duration (mins)` = 
            round(x = `mean duration (mins)`,
                  digits = 2),
         `median duration (mins)` = 
           round(x = `median duration (mins)`,
                 digits = 2)) %>% 
  
  # add date
  mutate(`Last updated` = Sys.Date())

## --------------------------------------------------------------------------------------
# specify account to pull from
gs4_auth(email = "globalgratitudecollaboration@gmail.com")

# worksheet name
ss = "https://docs.google.com/spreadsheets/d/13nRnKz6_VAiPSJmg7pHIy1ehNXtmrQUEhsCH7QHS2uI/edit?usp=sharing"

# update lab counts
sheet_write(data = summary,
            ss = ss,
            sheet = "DataTracker")

## --------------------------------------------------------------------------------------
## Data checks on our end

# completion
DF %>% 
  filter(consent == 1,
         Progress < 90) %>% 
  group_by(condition) %>% 
  summarise(m.prog = mean(Progress),
            n = n())

# reliability
for(l in c("POL_01", "DNK_01", "MYS_01")){
  d <- DF %>% 
    filter(lab == l,
           DistributionChannel != "preview",
           consent == 1)
  
  # calculate reliability statistics
  cbind(
    lab = l,
    pa = psych::alpha(x = d %>%
                        select(happy, satisfied, content, joyful, pleased))$total$raw_alpha,
    optimism = psych::alpha(x = d %>%
                              select(optimistic, hopeful))$total$raw_alpha,
    na = psych::alpha(x = d %>%
                        select(sad, depressed, anxious, nervous))$total$raw_alpha,
    indebt = psych::alpha(x = d %>%
                            select(indebted, obligated))$total$raw_alpha,
    envy = psych::alpha(x = d %>%
                          select(envious, bitter, jealous))$total$raw_alpha,
    swl = psych::alpha(x = d %>%
                         select(ls_1, ls_2, ls_3, ls_4, ls_5))$total$raw_alpha,
    self = psych::alpha(x = d %>%
                          select(self_image_circle, self_image))$total$raw_alpha) %>%
    as.data.frame() %>% 
    print()
}
