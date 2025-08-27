# 0. Install and load required packages ####
library(tidyverse)
library(haven)

# 1. Load raw AoW data ####

# a. Set the source folder path
source_folder <- 'G:/Shared drives/Curated Dataset/data/source' # Paste the filepath here

# b. Create a list of the files
file_list <- list.files(path = source_folder,
                        pattern = '\\.rds$',
                        full.names = TRUE)

# c. Load each file as data frame
for (file_path in file_list) {
  base_name <- basename(file_path)
  object_name <- substr(gsub('\\.rds$', '', base_name), 18, nchar(gsub('\\.rds$', '', base_name)))
  assign(object_name, readRDS(file_path))
}


# Rename age-in-months variables ####
# ...to distinguish between age-in-months variables from different dataframes.
# This is important because data may not all have been collected at the same time.

survey_mod231_main_dr24 <- survey_mod231_main_dr24 %>%
  rename(age_survey231_m = age_survey_m) # (new_variable_name = old_variable_variable)

survey_mod232_main_dr24 <- survey_mod232_main_dr24 %>%
  rename(age_survey232_m = age_survey_m) # (new_variable_name = old_variable_variable)

heightweight_dr24 <- heightweight_dr24 %>%
  rename(age_m_heightweight = age_m) # (new_variable_name = old_variable_variable)

skinfold_dr24 <- skinfold_dr24 %>%
  rename(age_m_skinfold = age_m) # (new_variable_name = old_variable_variable)

bloodpressure_dr24 <- bloodpressure_dr24 %>%
  rename(age_m_bloodpressure = age_m) # (new_variable_name = old_variable_variable)

bioimpedance_dr24 <- bioimpedance_dr24 %>%
  rename(age_m_bioimpedance = age_m) # (new_variable_name = old_variable_variable)


# Select participants who completed both survey modules 231 and 232 ####
aow_curated <- recruitment_dr24 %>% 
  filter(as_factor(has_survey_m231) == "Yes",
         as_factor(has_survey_m232) == "Yes")


# Add data from survey modules ####

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

# Identify repeated variables in recruitment and survey data (excluding recruitment ID)
repeated_vars1 <- intersect(names(aow_curated), names(survey_mod231_main_dr24)) %>%
  setdiff("aow_recruitment_id")
# Identify repeated variables in mod231 and mod232 data (excluding recruitment ID)
repeated_vars2 <- intersect(names(survey_mod231_main_dr24), names(survey_mod232_main_dr24)) %>%
  setdiff("aow_recruitment_id")

# Join both module dataframes excluding repeated variables
aow_curated <- aow_curated %>%
  left_join(
    survey_mod231_main_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id"
  ) %>%
  left_join(
    survey_mod232_main_dr24 %>% select(-any_of(repeated_vars2)),
    by = "aow_recruitment_id"
  )

# Filter data to only include 23-24 data (removing 22-23 data) ####
aow_curated <- aow_curated %>% 
  filter(recruitment_era == '2023-24')

# Exclude participants with age (months) difference between survey modules ####
# Compute difference in age (months) between survey modules
aow_curated$survey_age_diff <- (aow_curated$age_survey232_m - aow_curated$age_survey231_m)

# Filter participants with no age diff between modules
aow_curated <- aow_curated %>% 
  filter(survey_age_diff == '0')


#### Identify (and remove) duplicates ####
#implement rule for duplicated patient IDs (take data from earliest time point based on participant age in months)
aow_curated <- aow_curated %>% 
  group_by(aow_person_id) %>% 
  arrange(age_survey231_m) %>% 
  slice(1) %>% # Keep the first row in each group
  ungroup()


# Add height and weight data ####
# Identify repeated variables
repeated_vars1 <- intersect(names(aow_curated), names(heightweight_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
aow_curated <- aow_curated %>%
  left_join(
    heightweight_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id")


# Add skinfold data ####
# Identify repeated variables
repeated_vars1 <- intersect(names(aow_curated), names(skinfold_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
aow_curated <- aow_curated %>%
  left_join(
    skinfold_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id")


# Add bloodpressure data ####
# Identify repeated variables
repeated_vars1 <- intersect(names(aow_curated), names(bloodpressure_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
aow_curated <- aow_curated %>%
  left_join(
    bloodpressure_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id")


# Add bioimpedance data ####
# Identify repeated variables
repeated_vars1 <- intersect(names(aow_curated), names(bioimpedance_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
aow_curated <- aow_curated %>%
  left_join(
    bioimpedance_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id")






