# set up packages
library(here)
library(tidyverse)
library(psych)
library(lavaan)
library('semTools')

# open data
## specify directory
i_am("code/GlobalGratitude_Main.Rmd")

## fetch survey
DF <- 
  readRDS(file = here("data", "GlobalGratitude_Final_Cleaned.Rds"))%>% 
  
  # select user language and outcome variables
  select(UserLanguage,
         grateful, appreciative, thankful, 
         happy, satisfied, content, 
         joyful, pleased, optimistic,
         hopeful,  depressed, sad,  anxious,  nervous,
         indebted, obligated,
         envious, bitter, jealous,
         ls_1, ls_2, ls_3,
         ls_4, ls_5, , ladder,
         self_image_circle,
         self_image)

# look at language that may need to be dropped 
## to ensure sample covariance matrix is not positive-definite
DF %>% 
  group_by(UserLanguage) %>% 
  summarise(n = n()) %>% 
  arrange(n)

DF <- DF %>% 
  
  # remove missing data
  na.omit() %>% 
  
  # remove countries that are going to create modeling issues
  filter(UserLanguage != 'HI',
         UserLanguage != 'KAZ',
         UserLanguage != 'EL',
         UserLanguage != 'TH',
         UserLanguage != 'ZH-S',
         
         # round 2
         UserLanguage != 'TR',
         UserLanguage != 'ES', 
         UserLanguage != 'PT-BR', 
         UserLanguage != 'JA',
         
         # round 3
         UserLanguage != 'PL') #%>% 
  
  # scale data
  # mutate(across(where(is.numeric), 
  #               scale))

# determine number of factors
fa.parallel(DF %>% 
              select(-UserLanguage), 
            fa = "fa", 
            n.iter = 100, 
            show.legend = TRUE)

# look at factors
efa.6 <-
  fa(DF %>% 
       select(-UserLanguage),
     nfactors = 6,
     rotate = "oblimin", 
     fm = "ml")

fa.diagram(efa.6)

rm(efa.6)

efa.7 <-
  fa(DF %>% 
       select(-UserLanguage),
     nfactors = 7,
     rotate = "oblimin", 
     fm = "ml")

fa.diagram(efa.7)


# lavaan confirmatory approach
model <- '
  GR =~ grateful + thankful + appreciative
  PO =~ happy + joyful + pleased + satisfied + content
  OP =~ optimistic + hopeful
  NO =~ anxious + nervous + depressed + sad
  IN =~ indebted + obligated
  EN =~ envious + jealous + bitter
  SE =~ self_image + self_image_circle
  LS =~ ls_1 + ls_2 + ls_3 + ls_4 + ls_5 + ladder
'

model2 <- '
  PO =~ happy + joyful + pleased + satisfied + content
  NO =~ anxious + nervous + depressed + sad
  OP =~ optimistic + hopeful
  LS =~ ls_1 + ls_2 + ls_3 + ls_4 + ls_5 + ladder
  IN =~ indebted + obligated
  EN =~ envious + jealous + bitter
'

# Configural (no constraints)
fit_configural <- cfa(model, 
                      data = DF, 
                      group = "UserLanguage"
                      )

fit_configural <- cfa(model2, 
                      data = DF, 
                      group = "UserLanguage"
)
# Metric (constrain factor loadings)
fit_metric <- cfa(model, 
                  data = DF, 
                  group = "UserLanguage",
                  group.equal = "loadings"
                  )

fit_metric <- cfa(model2, 
                  data = DF, 
                  group = "UserLanguage",
                  group.equal = "loadings"
)

# Scalar (constrain loadings and intercepts)
fit_scalar <- cfa(model, 
                  data = DF, 
                  group = "UserLanguage",
                  group.equal = c("loadings", "intercepts")
                  )

fit_scalar <- cfa(model2, 
                  data = DF, 
                  group = "UserLanguage",
                  group.equal = c("loadings", "intercepts")
)

anova(fit_configural, fit_metric, fit_scalar)

measurementInvariance(model = model2, 
                      data = DF, 
                      group = "UserLanguage")
