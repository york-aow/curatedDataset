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

# Rename survey_version and survey_mode variables to distinguish between modules
survey_mod231_main_dr24 <- survey_mod231_main_dr24 %>%
  rename(
    survey231_version = survey_version,
    survey231_mode = survey_mode
  )
survey_mod232_main_dr24 <- survey_mod232_main_dr24 %>%
  rename(
    survey232_version = survey_version,
    survey232_mode = survey_mode
  )
# Rename age_survey_m to distinguish between modules (check they were completed at the same time)
survey_mod231_main_dr24 <- survey_mod231_main_dr24 %>%
  rename(
    age_survey231_m = age_survey_m,
  )
survey_mod232_main_dr24 <- survey_mod232_main_dr24 %>%
  rename(
    age_survey232_m = age_survey_m,
  )


# Identify repeated variables in recruitment and survey data (excluding recruitment ID)
repeated_vars1 <- intersect(names(both_mods_24), names(survey_mod231_main_dr24)) %>%
  setdiff("aow_recruitment_id")
# Identify repeated variables in mod231 and mod232 data (excluding recruitment ID)
repeated_vars2 <- intersect(names(survey_mod231_main_dr24), names(survey_mod232_main_dr24)) %>%
  setdiff("aow_recruitment_id")

# Join both module dataframes excluding repeated variables
both_mods_24 <- both_mods_24 %>%
  left_join(
    survey_mod231_main_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id"
  ) %>%
  left_join(
    survey_mod232_main_dr24 %>% select(-any_of(repeated_vars2)),
    by = "aow_recruitment_id"
  )

# Get list of variables in combined dataset
# Run Functions.R to create df_info function
df_info(both_mods_24, file = "both_mods_24.csv")
