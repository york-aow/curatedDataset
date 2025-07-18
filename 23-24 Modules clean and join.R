#### 23-24 Data cleaning and joining (derived datasets) ####

#Run loadData.R first

#Load required packages
library(tidyverse)

#### Module 1 ####

#filter data to only include 23-24 data (removing 22-23 data)
surveymod231 <- survey_mod231_derived_dr24 %>% 
  filter(recruitment_era == '2023-24')

#implement rule for duplicated patient IDs (take data from earliest time point based on participant age in months)
surveymod231.1 <- surveymod231 %>% 
  group_by(aow_person_id) %>% 
  arrange(age_survey_m) %>% 
  slice(1) %>% 
  ungroup()

#### Module 2 ####
surveymod232 <- survey_mod232_derived_dr24 %>% 
  filter(recruitment_era == '2023-24')

surveymod232.1 <- surveymod232 %>% 
  group_by(aow_person_id) %>% 
  arrange(age_survey_m) %>% 
  slice(1) %>% 
  ungroup()

#### Join ####

surveymod2312 <- left_join(survey_mod231.1, surveymod232.1, by = 'aow_person_id')
