#### 23-24 Data cleaning and joining (derived datasets) ####

#Run loadData.R first

#Load required packages
library(tidyverse)

#### Module 231 (derived) ####

#filter data to only include 23-24 data (removing 22-23 data)
surveymod231 <- survey_mod231_derived_dr24 %>% 
  filter(recruitment_era == '2023-24')

#implement rule for duplicated patient IDs (take data from earliest time point based on participant age in months)
surveymod231.1 <- surveymod231 %>% 
  group_by(aow_person_id) %>% 
  arrange(age_survey_m) %>% 
  slice(1) %>% # selects the first row in each group
  ungroup()

#### Module 232 (derived) ####
surveymod232 <- survey_mod232_derived_dr24 %>% 
  filter(recruitment_era == '2023-24')

surveymod232.1 <- surveymod232 %>% 
  group_by(aow_person_id) %>% 
  arrange(age_survey_m) %>% 
  slice(1) %>% 
  ungroup()

#### Join derived data ####

surveymod2312 <- left_join(surveymod231.1, surveymod232.1, by = 'aow_person_id')

length(unique(as.factor(surveymod2312$aow_person_id)))


#### 23-24 Data cleaning and joining (main datasets) ####

#### Module 231 (main) ####

#filter data to only include 23-24 data (removing 22-23 data)
surveymod231main24 <- survey_mod231_main_dr24 %>% 
  filter(recruitment_era == '2023-24')

#implement rule for duplicated patient IDs (take data from earliest time point based on participant age in months)
surveymod231main24unique <- surveymod231main24 %>% 
  group_by(aow_person_id) %>% 
  arrange(age_survey_m) %>% 
  slice(1) %>% # selects the first row in each group
  ungroup()

#### Module 232 (main) ####
surveymod232main24 <- survey_mod232_main_dr24 %>% 
  filter(recruitment_era == '2023-24')

surveymod232main24unique <- surveymod232 %>% 
  group_by(aow_person_id) %>% 
  arrange(age_survey_m) %>% 
  slice(1) %>% 
  ungroup()

#### Join main data ####

surveys_main_24 <- left_join(surveymod231main24unique, surveymod232main24unique, by = 'aow_person_id')

length(unique(as.factor(surveys_main_24$aow_person_id)))
