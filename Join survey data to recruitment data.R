#Run loadData.R first

#Load required packages
library(tidyverse)
library(haven)

print_labels(recruitment_dr24$has_survey_m231)
print_labels(recruitment_dr24$has_survey_m232)

# How many have both survey modules 231 and 232?
table(as_factor(recruitment_dr24$has_survey_m231), 
      as_factor(recruitment_dr24$has_survey_m232))
# 11,713

# Select only those who have both survey modules
both_mods_24 <- recruitment_dr24 %>% 
  filter(as_factor(has_survey_m231) == "Yes",
         as_factor(has_survey_m232) == "Yes")

# How many unique participant IDs?
length(unique(as.factor(both_mods_24$aow_person_id)))
# 10,034

# How many unique recruitment IDs?
length(unique(as.factor(both_mods_24$aow_recruitment_id)))
# 11,713


#### Add data from survey modules ####

# Identify repeated variables in recruitment and survey data (excluding recruitment ID)
repeated_vars <- intersect(names(both_mods_24), names(survey_mod231_main_dr24)) %>%
  setdiff("aow_recruitment_id")

# Join both module dataframes excluding repeated variables
both_mods_24_joined <- both_mods_24 %>%
  left_join(
    survey_mod231_main_dr24 %>% select(-any_of(repeated_vars)),
    by = "aow_recruitment_id"
  ) %>%
  left_join(
    survey_mod232_main_dr24 %>% select(-any_of(repeated_vars)),
    by = "aow_recruitment_id"
  )


