# Run loadData.R first

# Load required packages
library(tidyverse)
library(haven)

print_labels(recruitment_dr24$has_survey_m231)
print_labels(recruitment_dr24$has_survey_m232)

#### Exclude participants missing 231 or 232 ####

# How many have both survey modules 231 and 232?
table(as_factor(recruitment_dr24$has_survey_m231), 
      as_factor(recruitment_dr24$has_survey_m232))
# 11,713

# Select only those who have both survey modules
both_mods <- recruitment_dr24 %>% 
  filter(as_factor(has_survey_m231) == "Yes",
         as_factor(has_survey_m232) == "Yes")

# How many unique participant IDs?
length(unique(as.factor(both_mods$aow_person_id)))
# 10,034

# How many unique recruitment IDs?
length(unique(as.factor(both_mods$aow_recruitment_id)))
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
repeated_vars1 <- intersect(names(both_mods), names(survey_mod231_main_dr24)) %>%
  setdiff("aow_recruitment_id")
# Identify repeated variables in mod231 and mod232 data (excluding recruitment ID)
repeated_vars2 <- intersect(names(survey_mod231_main_dr24), names(survey_mod232_main_dr24)) %>%
  setdiff("aow_recruitment_id")

# Join both module dataframes excluding repeated variables
both_mods <- both_mods %>%
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
df_info(both_mods, file = "both_mods.csv")

#### Time between survey modules ####
# Is there any difference in age between surveys?
both_mods$survey_age_diff <- (both_mods$age_survey232_m - both_mods$age_survey231_m)
table(both_mods$survey_age_diff)
# Yes, there is...

#### Filter data to only include 23-24 data (removing 22-23 data) ####
table(both_mods$recruitment_era)
both_mods_24 <- both_mods %>% 
  filter(recruitment_era == '2023-24')
table(both_mods_24$recruitment_era)

#### Identify (and remove) duplicates ####
both_mods_24_duplicates <- both_mods_24 %>%
  group_by(aow_person_id) %>%
  filter(n() > 1) %>% # Keep groups that have more than one row
  ungroup()
#implement rule for duplicated patient IDs (take data from earliest time point based on participant age in months)
both_mods_24 <- both_mods_24 %>% 
  group_by(aow_person_id) %>% 
  arrange(age_survey231_m) %>% 
  slice(1) %>% # Keep the first row in each group
  ungroup()

# How many unique participant IDs?
length(unique(as.factor(both_mods_24$aow_person_id)))
# 7962

# How many unique recruitment IDs?
length(unique(as.factor(both_mods_24$aow_recruitment_id)))
# 7962

#### Exclude participants with age (months) difference between survey modules ####

# How many with age (months) difference between survey mod 231 and 232?
table(both_mods_24$survey_age_diff)
7962 - 7555 # 407

# Filter participants with no age diff between modules
both_mods_24_x <- both_mods_24 %>% 
  filter(survey_age_diff == '0')
table(both_mods_24_x$survey_age_diff)

#### Survey Version ####
table(both_mods_24_x$survey231_version) # All version 10
table(both_mods_24_x$survey232_version) # All version 10
# Survey mode
table(as_factor(both_mods_24_x$survey231_mode))
table(as_factor(both_mods_24_x$survey232_mode))
table(as_factor(both_mods_24_x$survey231_mode),
      as_factor(both_mods_24_x$survey232_mode)) # All completed both in the same mode


#### Add height and weight data ####

# Rename age in months at measurement to distinguish between dataframes
heightweight_dr24 <- heightweight_dr24 %>%
  rename(age_m_heightweight = age_m)

# Identify repeated variables
repeated_vars1 <- intersect(names(both_mods_24_x), names(heightweight_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
both_mods_24_x <- both_mods_24_x %>%
  left_join(
    heightweight_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id"
  )


#### Add skinfold data ####

# Rename age in months at measurement to distinguish between dataframes
skinfold_dr24 <- skinfold_dr24 %>%
  rename(age_m_skinfold = age_m)

# Identify repeated variables
repeated_vars1 <- intersect(names(both_mods_24_x), names(skinfold_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
both_mods_24_x <- both_mods_24_x %>%
  left_join(
    skinfold_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id"
  )


#### Add bloodpressure data ####

# Rename age in months at measurement to distinguish between dataframes
bloodpressure_dr24 <- bloodpressure_dr24 %>%
  rename(age_m_bloodpressure = age_m)

# Identify repeated variables
repeated_vars1 <- intersect(names(both_mods_24_x), names(bloodpressure_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
both_mods_24_x <- both_mods_24_x %>%
  left_join(
    bloodpressure_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id"
  )


#### Add bioimpedance data ####

# Rename age in months at measurement to distinguish between dataframes
bioimpedance_dr24 <- bioimpedance_dr24 %>%
  rename(age_m_bioimpedance = age_m)

# Identify repeated variables
repeated_vars1 <- intersect(names(both_mods_24_x), names(bioimpedance_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
both_mods_24_x <- both_mods_24_x %>%
  left_join(
    bioimpedance_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id"
  )

#### List all variables ####
# Run Functions.R to create df_info function
df_info(both_mods_24_x, file = "both_mods_24_x.csv")


#### Missing Data ####

# Percentage of responses NA on each variable
missing_data <- colMeans(is.na(both_mods_24))*100
# Save as csv
write.table(missing_data, file = "missing_table.csv", sep = ",")
# 84 variables have 100% missingness