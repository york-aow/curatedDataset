# File Setup Instructions ####
#
# To run this script, your files need to be in a specific structure.
# This project's main folder (the "root") should contain:
#
# 1. An RStudio Project file (.Rproj).
# 2. A 'data' folder.
#
# The 'data' folder must contain two sub-folders:
#   - 'derived': This is where the script will output processed data.
#   - 'source': contains the raw data files.
#
# The correct path should look like this:
# "your-project-folder"/
#  ├── "your-project".Rproj
#  └── data/
#      ├── derived/
#          └── (output files will be saved here)
#      └── source/
#          ├── BiB_AgeOfWonder__bioimpedance_dr23.rds
#          └── BiB_AgeOfWonder__bioimpedance_dr24.rds
#          └── BiB_AgeOfWonder__bloodpressure_dr23.rds
#          └── BiB_AgeOfWonder__bloodpressure_dr24.rds
#          └── BiB_AgeOfWonder__heightweight_dr23.rds
#          └── BiB_AgeOfWonder__heightweight_dr24.rds
#          └── BiB_AgeOfWonder__recruitment_dr23.rds
#          └── BiB_AgeOfWonder__recruitment_dr24.rds
#          └── BiB_AgeOfWonder__skinfold_dr23.rds
#          └── BiB_AgeOfWonder__skinfold_dr24.rds
#          └── BiB_AgeOfWonder__survey_mod01_dr23.rds
#          └── BiB_AgeOfWonder__survey_mod02_dr23.rds
#          └── BiB_AgeOfWonder__survey_mod03_dr23.rds
#          └── BiB_AgeOfWonder__survey_mod04_dr23.rds
#          └── BiB_AgeOfWonder__survey_mod231_derived_dr24.rds
#          └── BiB_AgeOfWonder__survey_mod231_main_dr24.rds
#          └── BiB_AgeOfWonder__survey_mod232_derived_dr24.rds
#          └── BiB_AgeOfWonder__survey_mod232_main_dr24.rds
#
# The 'here()' command automatically finds your main project folder
# and builds the path to 'data/source'.

# 0. Install and load required packages ####

# install tidyverse if not installed yet
if (!"tidyverse" %in% installed.packages()[, "Package"]) { # tidy R packages for data manipulation etc
  install.packages("tidyverse")
} 
# install haven if not installed yet
if (!"haven" %in% installed.packages()[, "Package"]) { # Import data of various formats
  install.packages("haven")
} 
# install psych if not installed yet
if (!"psych" %in% installed.packages()[, "Package"]) { # Function to pro-rate missing items (item.scores)
  install.packages("psych")
}
# install here if not installed yet
if (!"here" %in% installed.packages()[, "Package"]) { # package for easy file referencing
  install.packages("here")
}

# install labelled if not installed yet
if (!"labelled" %in% installed.packages()[, "Package"]) { # package for easy file referencing
  install.packages("labelled")
}

library(tidyverse) # tidy R packages for data manipulation etc
library(haven) # Import data of various formats
library(psych) # Function to pro-rate missing items (item.scores)
library(here) # package for easy file referencing
library(labelled) # Function to remove unused value labels from haven_labelled variables

# 1. Load raw AoW data ####

# a. Set the source folder path automatically using the 'here' package
# Make sure you have a project file (.Rproj) or .git in your root directory.

source_folder <- here('data', 'source')

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

# 2. Join and filter data ####

## Rename age-in-months variables ####
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


## Select participants who completed both survey modules 231 and 232 ####
aow_curated <- recruitment_dr24 %>% 
  filter(as_factor(has_survey_m231) == "Yes",
         as_factor(has_survey_m232) == "Yes")


## Add data from survey modules ####

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

## Filter data to only include 23-24 data (removing 22-23 data) ####
aow_curated <- aow_curated %>% 
  filter(recruitment_era == '2023-24')

## Exclude participants with age (months) difference between survey modules ####
# Compute difference in age (months) between survey modules
aow_curated$survey_age_diff <- (aow_curated$age_survey232_m - aow_curated$age_survey231_m)

# Filter participants with no age diff between modules
aow_curated <- aow_curated %>% 
  filter(survey_age_diff == '0')


## Identify (and remove) duplicates ####
#implement rule for duplicated patient IDs (take data from earliest time point based on participant age in months)
aow_curated <- aow_curated %>% 
  group_by(aow_person_id) %>% 
  arrange(age_survey231_m) %>% 
  slice(1) %>% # Keep the first row in each group
  ungroup()


## Add height and weight data ####
# Identify repeated variables
repeated_vars1 <- intersect(names(aow_curated), names(heightweight_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
aow_curated <- aow_curated %>%
  left_join(
    heightweight_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id")


## Add skinfold data ####
# Identify repeated variables
repeated_vars1 <- intersect(names(aow_curated), names(skinfold_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
aow_curated <- aow_curated %>%
  left_join(
    skinfold_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id")


## Add bloodpressure data ####
# Identify repeated variables
repeated_vars1 <- intersect(names(aow_curated), names(bloodpressure_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
aow_curated <- aow_curated %>%
  left_join(
    bloodpressure_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id")


## Add bioimpedance data ####
# Identify repeated variables
repeated_vars1 <- intersect(names(aow_curated), names(bioimpedance_dr24)) %>%
  setdiff("aow_recruitment_id")
# Join dataframes excluding repeated variables
aow_curated <- aow_curated %>%
  left_join(
    bioimpedance_dr24 %>% select(-any_of(repeated_vars1)),
    by = "aow_recruitment_id")

# Note: Data were not all collected at the same time, see differences in age (months)
# between measurements (see 'Useful commands.R')

# 3. Remove redundant variables ####

aow_curated <- aow_curated %>% 
  select(
    -aow_person_id, # aow_recruitment_id is the unique identifier
    -BiBPersonID, # Not relevant
    -is_bib, # Not relevant
    -gender, # Use self-reported gender instead
    -ethnicity_1, # Use self-reported ethnicity instead
    -ethnicity_2, # Use self-reported ethnicity instead
    -LSOA11CD,
    -IMD_2019_score, #IMD Decile more useful
    -birth_year, # Unnecessary, age in years variable in data
    -birth_month, # Unnecessary, age in months variable in data
    -recruitment_era, # All 2023-2024
    -recruitment_year, # either 2023 or 2024
    -recruitment_month, # not relevant
    -age_recruitment_y, # age at time of data collection more important
    -age_recruitment_m, # age at time of data collection more important
    -has_bioimp, # not necessary 
    -has_bp, # not necessary 
    -has_skinfld, # not necessary 
    -has_htwt, # not necessary 
    -has_survey_m231, # all included observations have a survey
    -has_survey_m232, # all included observations have a survey
    -has_survey_m1, # all included observations have a survey
    -has_survey_m2, # all included observations have a survey
    -has_survey_m3, # all included observations have a survey
    -has_survey_m4, # all included observations have a survey
    -has_survey, # all included observations have a survey
    -has_data, # all included observations have some AoW data
    -aw1_2_years_lvd_r4, # 100% missing, alternative version (aw1_2_years_lvd_a4) with responses
    -awb1_2_ethnicity_arb_r4, # 100% missing, alternative version (awb1_2_ethnicity_arb_a4) with responses.
    -awb3_1y_save_mny_a5___1, # 100% missing, alternative version (r10) with responses
    -awb3_1y_save_mny_a5___2, # 100% missing, alternative version (r10) with responses
    -awb3_1y_save_mny_a5___3, # 100% missing, alternative version (r10) with responses
    -awb3_1y_save_mny_a5___4, # 100% missing, alternative version (r10) with responses
    -awb3_1y_save_mny_a5___5, # 100% missing, alternative version (r10) with responses
    -awb3_1y_save_mny_a5___6, # 100% missing
    -awb3_1y_save_mny_othr_a5, # 100% missing
    -awb3_2_homes_1_ppl___1, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___2, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___3, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___4, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___5, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___6, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___7, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___8, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___9, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___10, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___11, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___12, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___13, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___14, # 100% missing, alternative version (r10) with responses
    -awb3_2_homes_1_ppl___15, # 100% missing, alternative version (r10) with responses
    -aw3_6_comparison_2, # 100% missing, alternative version (aw3_6_comparison_2_r10) with responses
    -awb3_activities_3_r5, # 100% missing, alternative version (awb3_activities_3_r10) with responses
    -awb3_activities_15_r5, # 100% missing, alternative version (awb3_activities_15_r10) with responses
    -awb3_activities_11_r5, # 100% missing, alternative version (awb3_activities_11_r10) with responses
    -awb3_activities_17_r5, # 100% missing, alternative version (awb3_activities_17_r10) with responses
    -awb3_activities_18_r5, # 100% missing, alternative version (awb3_activities_18_r10) with responses
    -awb3_activities_6_r5, # 100% missing, alternative version (awb3_activities_6_r10) with responses
    -awb3_activities_14_r5, # 100% missing, alternative version (awb3_activities_14_r10) with responses
    -awb3_activities_16_r5, # 100% missing, alternative version (awb3_activities_16_r10) with responses
    -awb3_activities_1_r5, # 100% missing, alternative version (awb3_activities_1_r10) with responses
    -awb3_activities_10_r5, # 100% missing, alternative version (awb3_activities_10_r10) with responses
    -awb3_activities_12_r5, # 100% missing, alternative version (awb3_activities_12_r10) with responses
    -awb3_activities_13_r5, # 100% missing, alternative version (awb3_activities_13_r10) with responses
    -awb3_activities_2_r5, # 100% missing, alternative version (awb3_activities_2_r10) with responses
    -awb3_activities_4_r5, # 100% missing, alternative version (awb3_activities_4_r10) with responses
    -awb3_activities_5_r5, # 100% missing, alternative version (awb3_activities_5_r10) with responses
    -awb3_activities_7_r5, # 100% missing, alternative version (awb3_activities_7_r10) with responses
    -awb3_activities_8_r5, # 100% missing, alternative version (awb3_activities_8_r10) with responses
    -awb3_activities_9_r5, # 100% missing, alternative version (awb3_activities_9_r10) with responses
    -awb5_1_hearing_sght_1, # 100% missing, alternative version (awb5_1_hearing_sght_1_r10, Have you been told to, or do you need to wear glasses in order to see clearly?) with responses
    -awb5_1_hearing_sght_n_a10, # Short or long sighted, unlikely to be useful
    -awb5_1_hearing_sght_y1_a10, # Reason for wearing glassess, unlikely to be useful
    -awb5_1_hearing_sght_y2_a10, # Age first wore glasses, unlikely to be useful
    -awb5_1_cigs2_a5, # 100% missing, alternative version (awb5_1_cigs2_r10) with responses
    -awb5_2_vape_r4, # 100% missing, alternative version (awb5_2_vape_r10) with responses
    -awb5_2_drugs_11, # 100% missing, NA
    -awb5_2_drugs_13, # 100% missing, NA
    -awb5_2_drugs_14, # 100% missing, NA
    -awb5_2_drugs_16, # 100% missing, NA
    -awb5_2_gambling_2_a5, # 100% missing, alternative version (awb5_2_gambling_2_r10, Gambling: Last spent money (National Lottery games)) with responses
    -awb5_2_gambling_6_a5, # 100% missing, alternative version (awb5_2_gambling_6_r10, Gambling: Last spent money (Slot machines)) with responses
    -awb5_2_gambling_7_a5, # 100% missing, alternative version (awb5_2_gambling_7_r10, Gambling: Last spent money (Private bets for money)) with responses
    -awb5_2_gambling_8_a5, # 100% missing, alternative version (awb5_2_gambling_8_r10, Gambling: Last spent money (Playing cards for money)) with responses
    -awb5_2_gambling_9_a5, # 100% missing, alternative version (awb5_2_gambling_9_r10, Gambling: Last spent money (Bingo at bingo club)) with responses
    -awb5_2_gambling_10_a5, # 100% missing, alternative version (awb5_2_gambling_10_r10, Gambling: Last spent money (Bingo not at a bingo club)) with responses
    -awb5_2_gambling_11_a5, # 100% missing, alternative version (awb5_2_gambling_11_r10, Gambling: Last spent money (Gaming machines in betting shop)) with responses
    -awb5_2_gambling_12_a5, # 100% missing, alternative version (awb5_2_gambling_12_r10, Gambling: Last spent money (Placing a bet at a betting shop)) with responses
    -awb5_2_gambling_13_a5, # 100% missing, alternative version (awb5_2_gambling_13_r10, Gambling: Last spent money (Visiting a casino to play casino games)) with responses
    -awb5_2_gambling_14_a5, # 100% missing, alternative version (awb5_2_gambling_14_r10, Gambling: Last spent money (Gambling websites/apps with cash prizes)) with responses
    -awb7_1_bullying_1_on, # 100% missing, alternative question (awb7_1_bullying_1, 'Is bullying a problem at your school?') with responses
    -awb7_1_bullying_1_off, # 100% missing, alternative question (awb7_1_bullying_1, 'Is bullying a problem at your school?') with responses
    -awb7_1_safe1_r8, # 100% missing, alternative question (awb7_1_safe_r8, 'I feel safe when I am at school') with responses
    -awb2_7_friends_physcl_a5, # 100% missing, alternative question (awb2_7_friends_physcl_r10, 'How many in-person friends do you have?') with responses
    -awb2_7_friends_onln_a5, # 100% missing, alternative question (awb2_7_friends_onln_r10, 'How many online friends do you have?') with responses
    -awb2_7_friends_clse_1_a5, # 100% missing
    -awb2_7_friends_clse_2_a5, # 100% missing
    -awb8_1_morality_1, # 100% missing, alternative question (awb8_1_morality_1_r10, 'Do people your age: Start a fight with someone?') with responses
    -awb8_1_morality_2, # 100% missing, alternative question (awb8_1_morality_2_r10, 'Do people your age: Write things/spray paint on a building, fence, train') with responses
    -awb8_1_morality_3, # 100% missing, alternative question (awb8_1_morality_3_r10, 'Do people your age: Take something from a shop without paying for it') with responses
    -awb8_1_morality_4, # 100% missing, alternative question (awb8_1_morality_4_r10, 'Do people your age: Copy/download music/games/films without paying') with responses
    -awb6_1_social_media___1, # 100% missing with alternative
    -awb6_1_social_media___2, # 100% missing with alternative
    -awb6_1_social_media___3, # 100% missing with alternative
    -awb6_1_social_media___4, # 100% missing with alternative
    -awb6_1_social_media___5, # 100% missing with alternative
    -awb6_1_social_media___6, # 100% missing with alternative
    -awb6_1_social_media___7, # 100% missing with alternative
    -awb6_1_social_media___8, # 100% missing with alternative
    -awb6_1_social_media___9, # 100% missing with alternative
    -awb6_1_social_media___10, # 100% missing with alternative
    -awb6_1_social_media___11, # 100% missing with alternative
    -awb6_6_int_hme_hrs, # 100% missing, removed
    -awb6_6_int_hme_hrs_wknd, # 100% missing, removed
    -survey_age_diff, # all observations included have 0 age difference
    -age_survey232_m, # same as age_survey231_m
    -has_measure, # unnecessary
    -survey231_version, # all version 10
    -survey232_version, # all version 10
    -survey232_mode, # same as survey 231 mode
    -awb1_2_ethnicity_othr_othr, # free text responses
    -awb1_2_language_hme_othr, # free text responses
    -awb1_2y_religion_othr, # free text responses
    -awb1_2_sex_othr, # free text responses
    -awb1_2_gender_othr_r4, # free text responses
    -awb3_2_homes_1_ppl_othr, # free text responses
    -awb5_2_drugs_othr2_a10, # free text responses
    -awb4_1y_sick_a5, # free text responses
    -awb2_9_seek_hlp_ppl_11, # free text responses
    -awb8_2_club_rsn_2, # free text responses
    -awb8_2_excl_rsn_2, # free text responses
    -awb8_2_age_rsn_2, # free text responses
    -awb8_2_lang_rsn_2, # free text responses
    -awb8_2_police_rsn_2, # free text responses
    -awb8_2_shop_rsn_2, # free text responses
    -awb8_2_names_rsn_2, # free text responses
    -awb8_2_service_rsn_2, # free text responses
    -awb8_2_int_rsn_2, # free text responses
    -awb8_2_afraid_rsn_2, # free text responses
    -awb8_2_threat_rsn_2, # free text responses
    -awb6_1_social_media_othr, # free text responses
    -awb6_1_positive_exp_othr, # free text responses
    -awb6_1_neg_exp_othr_r5, # free text responses
    -age_y, # this is from the heightweight data, only need age in months to check time between data collection 
    -awb1_2_ethnicity_whte, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb1_2_ethnicity_whte_othr, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb1_2_ethnicity_mix, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb1_2_ethnicity_mix_othr, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb1_2_ethnicity_asn, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb1_2_ethnicity_asn_othr, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb1_2_ethnicity_blck, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb1_2_ethnicity_blck_afrcn, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb1_2_ethnicity_whte_crrbn, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb1_2_ethnicity_arb_a4, # awb1_2_ethnicity_r4 gives a value for ethnicity categories
    -awb3_1y_save_mny_r10___1, # unclear whether missing or genuine 0 responses. Alternative questions cover this theme
    -awb3_1y_save_mny_r10___2, # unclear whether missing or genuine 0 responses. Alternative questions cover this theme
    -awb3_1y_save_mny_r10___3, # unclear whether missing or genuine 0 responses. Alternative questions cover this theme
    -awb3_1y_save_mny_r10___4, # unclear whether missing or genuine 0 responses. Alternative questions cover this theme
    -awb3_1y_save_mny_r10___5 # unclear whether missing or genuine 0 responses. Alternative questions cover this theme
  )

# 4. Compute scale total scores ####

## Create recode functions ####

# Recode Likert scale 1-4 to 0-3
recode14_03 <- function(x) {
  case_match(x,
             1 ~ 0,
             2 ~ 1,
             3 ~ 2,
             4 ~ 3,
             NA ~ NA)
}

# Reverse code 0-2 Likert scale
recode02_20 <- function(x) {
  case_match(x,
             0 ~ 2,
             1 ~ 1,
             2 ~ 0,
             NA ~ NA)
}

# Reverse code 1-5 Likert scale
recode15_51 <- function(x) {
  case_match(x,
             1 ~ 5,
             2 ~ 4,
             3 ~ 3,
             4 ~ 2,
             5 ~ 1,
             NA ~ NA)
}


## EDE-QS ####

# Create list of items
edeqs_items <-
  c("awb2_12_eat_hbt_1_a5",
    "awb2_12_eat_hbt_2_a5",
    "awb2_12_eat_hbt_3_a5",
    "awb2_12_eat_hbt_4_a5",
    "awb2_12_eat_hbt_5_a5",
    "awb2_12_eat_hbt_6_a5",
    "awb2_12_eat_hbt_7_a5",
    "awb2_12_eat_hbt_8_a5",
    "awb2_12_eat_hbt_9_a5",
    "awb2_12_eat_hbt_10_a5",
    "awb2_12_wght_1_a5",
    "awb2_12_wght_2_a5")

# Recode item responses from 1-4 to 0-3
aow_curated <- aow_curated %>%
  mutate(across(all_of(edeqs_items), recode14_03))

# For those that responded 0 for item 9, impute 0 for item 10
aow_curated <- aow_curated %>%
  mutate(awb2_12_eat_hbt_10_a5 = ifelse(awb2_12_eat_hbt_9_a5 == 0 & is.na(awb2_12_eat_hbt_10_a5),
                                        0, awb2_12_eat_hbt_10_a5))

# Identify complete EDE-QS responses
aow_curated <- aow_curated %>%
  mutate(edeqs_complete = rowSums(is.na(select(., !!!edeqs_items))) == 0) # !!! removes quote marks from the list items

# Compute total scores for those with complete EDE-QS, and assign NA to those without complete edeqs
aow_curated <- aow_curated %>%
  mutate(edeqs_total = ifelse(edeqs_complete, rowSums(select(., !!!edeqs_items), na.rm = TRUE), NA))

## RCADS ####

# Create list of rcads items
rcads_items <- c("awb2_1_illhealth_1",
                 "awb2_1_illhealth_2",
                 "awb2_1_illhealth_3",
                 "awb2_1_illhealth_4",
                 "awb2_1_illhealth_5",
                 "awb2_1_illhealth_6",
                 "awb2_1_illhealth_7",
                 "awb2_1_illhealth_8",
                 "awb2_1_illhealth_9",
                 "awb2_1_illhealth_10",
                 "awb2_1_illhealth_11",
                 "awb2_1_illhealth_12",
                 "awb2_1_illhealth_13",
                 "awb2_1_illhealth_14",
                 "awb2_1_illhealth_15",
                 "awb2_1_illhealth_16",
                 "awb2_1_illhealth_17",
                 "awb2_1_illhealth_18",
                 "awb2_1_illhealth_19",
                 "awb2_1_illhealth_20",
                 "awb2_1_illhealth_21",
                 "awb2_1_illhealth_22",
                 "awb2_1_illhealth_23",
                 "awb2_1_illhealth_24",
                 "awb2_1_illhealth_25")

# Create list of depression items
rcads_dep_items <- c("awb2_1_illhealth_1",
                     "awb2_1_illhealth_4",
                     "awb2_1_illhealth_8",
                     "awb2_1_illhealth_10",
                     "awb2_1_illhealth_13",
                     "awb2_1_illhealth_15",
                     "awb2_1_illhealth_16",
                     "awb2_1_illhealth_19",
                     "awb2_1_illhealth_21",
                     "awb2_1_illhealth_24")

# Create list of anxiety items
rcads_anx_items <- c("awb2_1_illhealth_2",
                     "awb2_1_illhealth_3",
                     "awb2_1_illhealth_5",
                     "awb2_1_illhealth_6",
                     "awb2_1_illhealth_7",
                     "awb2_1_illhealth_9",
                     "awb2_1_illhealth_11",
                     "awb2_1_illhealth_12",
                     "awb2_1_illhealth_14",
                     "awb2_1_illhealth_17",
                     "awb2_1_illhealth_18",
                     "awb2_1_illhealth_20",
                     "awb2_1_illhealth_22",
                     "awb2_1_illhealth_23",
                     "awb2_1_illhealth_25")

# Recode Likert scale from 1-4 to 0-3
aow_curated <- aow_curated %>%
  mutate(across(all_of(rcads_anx_items), recode14_03))

aow_curated <- aow_curated %>%
  mutate(across(all_of(rcads_dep_items), recode14_03))

# Identify complete responses
aow_curated <- aow_curated %>%
  mutate(rcads_dep_complete = rowSums(is.na(select(., !!!rcads_dep_items))) == 0) # !!! removes quote marks from the list items

aow_curated <- aow_curated %>%
  mutate(rcads_anx_complete = rowSums(is.na(select(., !!!rcads_anx_items))) == 0) # !!! removes quote marks from the list items

# Compute total scores for those with complete RCADS subscale, and assign NA to those with incomplete
aow_curated <- aow_curated %>%
  mutate(rcads_dep_total = ifelse(rcads_dep_complete, rowSums(select(., !!!rcads_dep_items), na.rm = TRUE), NA))

aow_curated <- aow_curated %>%
  mutate(rcads_anx_total = ifelse(rcads_anx_complete, rowSums(select(., !!!rcads_anx_items), na.rm = TRUE), NA))

# Create RCADS scale total score (sum depression and anxiety, leave NA for any missing either subscale)
aow_curated$rcads_total <- 
  ifelse(is.na(aow_curated$rcads_anx_total) | is.na(aow_curated$rcads_dep_total), NA, 
         aow_curated$rcads_anx_total + aow_curated$rcads_dep_total)

## SWEMWBS ####

# Create a list of SWEMWBS items
swemwbs_items <- c("awb2_2_optmstc_1_a4",
                   "awb2_2_useful_2_a4",
                   "awb2_2_relxed_3_a4",
                   "awb2_2_problems_4_a4",
                   "awb2_2_think_clr_5_a4",
                   "awb2_2_close_othrs_6_a4",
                   "awb2_2_own_mnd_7_a4")

# Identify complete SWEMWBS responses
aow_curated <- aow_curated %>%
  mutate(swemwbs_complete = rowSums(is.na(select(., !!!swemwbs_items))) == 0)

# Compute total scores for those with complete SWEMWBS, and assign NA to those without
aow_curated <- aow_curated %>%
  mutate(swemwbs_total = ifelse(swemwbs_complete, rowSums(select(., !!!swemwbs_items), na.rm = TRUE), NA))


## SDQ ####

# List items

sdq_items <- c("awb2_1_sdq_1_a10",
               "awb2_1_sdq_2_a10",
               "awb2_1_sdq_3_a10",
               "awb2_1_sdq_4_a10",
               "awb2_1_sdq_5_a10",
               "awb2_1_sdq_6_a10",
               "awb2_1_sdq_7_a10",
               "awb2_1_sdq_8_a10",
               "awb2_1_sdq_9_a10",
               "awb2_1_sdq_10_a10",
               "awb2_1_sdq_11_a10",
               "awb2_1_sdq_12_a10",
               "awb2_1_sdq_13_a10",
               "awb2_1_sdq_14_a10",
               "awb2_1_sdq_15_a10",
               "awb2_1_sdq_16_a10",
               "awb2_1_sdq_17_a10",
               "awb2_1_sdq_18_a10",
               "awb2_1_sdq_19_a10",
               "awb2_1_sdq_20_a10",
               "awb2_1_sdq_21_a10",
               "awb2_1_sdq_22_a10",
               "awb2_1_sdq_23_a10",
               "awb2_1_sdq_24_a10",
               "awb2_1_sdq_25_a10")

sdq_emo_items <- c("awb2_1_sdq_3_a10",
                   "awb2_1_sdq_8_a10",
                   "awb2_1_sdq_13_a10",
                   "awb2_1_sdq_16_a10",
                   "awb2_1_sdq_24_a10")

sdq_con_items <- c("awb2_1_sdq_5_a10",
                   "awb2_1_sdq_7_a10", # This item is reverse coded
                   "awb2_1_sdq_12_a10",
                   "awb2_1_sdq_18_a10",
                   "awb2_1_sdq_22_a10")

sdq_hyp_items <- c("awb2_1_sdq_2_a10",
                   "awb2_1_sdq_10_a10", 
                   "awb2_1_sdq_15_a10",
                   "awb2_1_sdq_21_a10", # This item is reverse coded
                   "awb2_1_sdq_25_a10") # This item is reverse coded

sdq_pee_items <- c("awb2_1_sdq_6_a10",
                   "awb2_1_sdq_11_a10", # This item is reverse coded
                   "awb2_1_sdq_14_a10", # This item is reverse coded
                   "awb2_1_sdq_19_a10", 
                   "awb2_1_sdq_23_a10")

sdq_pro_items <- c("awb2_1_sdq_1_a10",
                   "awb2_1_sdq_4_a10", 
                   "awb2_1_sdq_9_a10", 
                   "awb2_1_sdq_17_a10", 
                   "awb2_1_sdq_20_a10")

sdq_int_items <- c(sdq_emo_items, sdq_pee_items)

sdq_ext_items <- c(sdq_con_items, sdq_hyp_items) 

sdq_dif_items <- c(sdq_int_items, sdq_ext_items)

sdq_items <- c(sdq_dif_items, sdq_pro_items)

sdq_reverse_items <- c("awb2_1_sdq_7_a10", # This item is reverse coded
                       "awb2_1_sdq_21_a10", # This item is reverse coded
                       "awb2_1_sdq_25_a10", # This item is reverse coded
                       "awb2_1_sdq_11_a10", # This item is reverse coded
                       "awb2_1_sdq_14_a10") # This item is reverse coded

# Recode reverse scored item responses from 0-2 to 2-0
aow_curated <- aow_curated %>%
  mutate(across(all_of(sdq_reverse_items), recode02_20))

# Identify complete subscale responses
aow_curated <- aow_curated %>%
  mutate(sdq_con_complete = rowSums(is.na(select(., !!!sdq_con_items))) == 0)

aow_curated <- aow_curated %>%
  mutate(sdq_emo_complete = rowSums(is.na(select(., !!!sdq_emo_items))) == 0)

aow_curated <- aow_curated %>%
  mutate(sdq_hyp_complete = rowSums(is.na(select(., !!!sdq_hyp_items))) == 0)

aow_curated <- aow_curated %>%
  mutate(sdq_pee_complete = rowSums(is.na(select(., !!!sdq_pee_items))) == 0)

aow_curated <- aow_curated %>%
  mutate(sdq_pro_complete = rowSums(is.na(select(., !!!sdq_pro_items))) == 0)

# Compute subscale total scores for those with complete subscale, and assign NA to those without
aow_curated <- aow_curated %>%
  mutate(sdq_con_total = ifelse(sdq_con_complete, rowSums(select(., !!!sdq_con_items), na.rm = TRUE), NA)) %>%
  mutate(sdq_emo_total = ifelse(sdq_emo_complete, rowSums(select(., !!!sdq_emo_items), na.rm = TRUE), NA)) %>%
  mutate(sdq_hyp_total = ifelse(sdq_hyp_complete, rowSums(select(., !!!sdq_hyp_items), na.rm = TRUE), NA)) %>%
  mutate(sdq_pee_total = ifelse(sdq_pee_complete, rowSums(select(., !!!sdq_pee_items), na.rm = TRUE), NA)) %>%
  mutate(sdq_pro_total = ifelse(sdq_pro_complete, rowSums(select(., !!!sdq_pro_items), na.rm = TRUE), NA))

# Compute internalising and externalising scores

aow_curated$sdq_ext_total <- 
  ifelse(is.na(aow_curated$sdq_con_total) | is.na(aow_curated$sdq_hyp_total), NA, 
         aow_curated$sdq_con_total + aow_curated$sdq_hyp_total)

aow_curated$sdq_int_total <- 
  ifelse(is.na(aow_curated$sdq_emo_total) | is.na(aow_curated$sdq_pee_total), NA, 
         aow_curated$sdq_emo_total + aow_curated$sdq_pee_total)

# Compute total difficulties 

aow_curated$sdq_dif_total <- 
  ifelse(is.na(aow_curated$sdq_ext_total) | is.na(aow_curated$sdq_int_total), NA, 
         aow_curated$sdq_ext_total + aow_curated$sdq_int_total)


## UCLA-3 ####

ucla3_items <- c("awb2_4_loneliness_1",
                 "awb2_4_loneliness_2",
                 "awb2_4_loneliness_3")

# Identify complete UCLA-3 responses
aow_curated <- aow_curated %>%
  mutate(ucla3_complete = rowSums(is.na(select(., !!!ucla3_items))) == 0)

# Compute total scores for those with complete UCLA-3, and assign NA to those without
aow_curated <- aow_curated %>%
  mutate(ucla3_total = ifelse(ucla3_complete, rowSums(select(., !!!ucla3_items), na.rm = TRUE), NA))


## UCLA-4 ####

ucla4_items <- c("awb2_4_loneliness_1",
                 "awb2_4_loneliness_2",
                 "awb2_4_loneliness_3",
                 "awb2_4_loneliness_4")

# Identify complete UCLA-4 responses
aow_curated <- aow_curated %>%
  mutate(ucla4_complete = rowSums(is.na(select(., !!!ucla4_items))) == 0)

# Compute total scores for those with complete UCLA-4, and assign NA to those without
aow_curated <- aow_curated %>%
  mutate(ucla4_total = ifelse(ucla4_complete, rowSums(select(., !!!ucla4_items), na.rm = TRUE), NA))


## BRS ####

brs_items <- c("awb2_9_resil1_a5",
               "awb2_9_resil2_a5", # This item is reverse coded
               "awb2_9_resil3_a5",
               "awb2_9_resil4_a5", # This item is reverse coded
               "awb2_9_resil5_a5",
               "awb2_9_resil6_a5") # This item is reverse coded

brs_reverse_items <- c("awb2_9_resil2_a5", # This item is reverse coded
                       "awb2_9_resil4_a5", # This item is reverse coded
                       "awb2_9_resil6_a5") # This item is reverse coded

# Recode reverse scored item responses from 0-2 to 2-0
aow_curated <- aow_curated %>%
  mutate(across(all_of(brs_reverse_items), recode15_51))

# Identify complete BRS responses
aow_curated <- aow_curated %>%
  mutate(brs_complete = rowSums(is.na(select(., !!!brs_items))) == 0)

# Compute total scores for those with complete BRS, and assign NA to those without
aow_curated <- aow_curated %>%
  mutate(brs_total = ifelse(brs_complete, rowSums(select(., !!!brs_items), na.rm = TRUE), NA))


## YAP-S ####

yaps_items <- c("awb4_2_outside_schl_1_r7",
                "awb4_2_outside_schl_2_r7",
                "awb4_2_outside_schl_3_r7",
                "awb4_2_outside_schl_4_r7",
                "awb4_2_overall_a5")

# Identify complete YAP-S responses
aow_curated <- aow_curated %>%
  mutate(yaps_complete = rowSums(is.na(select(., !!!yaps_items))) == 0)

# Compute total scores for those with complete YAP-S, and assign NA to those without
aow_curated <- aow_curated %>%
  mutate(yaps_total = ifelse(yaps_complete, rowSums(select(., !!!yaps_items), na.rm = TRUE), NA))

# 5. Rename variables ####

# Create lists of new variable names
rcads_items_new <- paste("rcads_item", seq_along(rcads_items), sep = "_")
sdq_items_new <- paste("sdq_item", seq_along(sdq_items), sep = "_")
edeqs_items_new <- paste("edeqs_item", seq_along(edeqs_items), sep = "_")
swemwbs_items_new <- paste("swemwbs_item", seq_along(swemwbs_items), sep = "_")
brs_items_new <- paste("brs_item", seq_along(brs_items), sep = "_")
yaps_items_new <- paste("yaps_item", seq_along(yaps_items), sep = "_")

# Rename the variables using the lists
# Create one combined vector for all renaming rules
all_renames <- c(
  setNames(rcads_items, rcads_items_new), #setNames(old name, new name)
  setNames(sdq_items, sdq_items_new),
  setNames(edeqs_items, edeqs_items_new),
  setNames(swemwbs_items, swemwbs_items_new),
  setNames(brs_items, brs_items_new),
  setNames(yaps_items, yaps_items_new)
)

# Apply all renames in a single step
aow_curated <- aow_curated %>%
  rename(!!!all_renames)

## Rename other variables inc those from incomplete or altered scales ####
# (new name = old name)
aow_curated <- aow_curated %>%

  rename(
    # Loneliness
    loneliness_ucla3_item_1 = awb2_4_loneliness_1,
    loneliness_ucla3_item_2 = awb2_4_loneliness_2,
    loneliness_ucla3_item_3 = awb2_4_loneliness_3,
    loneliness_ons_item     = awb2_4_loneliness_4,
    
    # Help Seeking (GHSQ)
    helpseek_ghsq_item_1a        = awb2_9_seek_hlp_ppl_1_r4, # partner
    helpseek_ghsq_item_1b        = awb2_9_seek_hlp_ppl_2, # friend
    helpseek_ghsq_item_1c        = awb2_9_seek_hlp_ppl_3, # parent
    helpseek_ghsq_item_1d        = awb2_9_seek_hlp_ppl_4, # relative
    helpseek_ghsq_item_1e        = awb2_9_seek_hlp_ppl_5, # mental health professional
    helpseek_ghsq_item_1f        = awb2_9_seek_hlp_ppl_6, # helpline
    helpseek_ghsq_item_1g        = awb2_9_seek_hlp_ppl_7, # doctor
    helpseek_ghsq_item_1h        = awb2_9_seek_hlp_ppl_8, # religious leader
    helpseek_ghsq_item_1i        = awb2_9_seek_hlp_ppl_10, # no-one
    helpseek_ghsq_item_1j_binary = awb2_9_seek_hlp_ppl_othr_a3, # other, binary
    
    # Help Seeking (other)
    helpseek_teacher = awb2_9_seek_hlp_ppl_9_a_4, # teacher
    
    # Psychosis-like Symptoms (PLIKS)
    pliks_heard_voices             = awb2_11_psychosis_3_r4,
    pliks_heard_voices_distress    = awb2_11_upsetting_3_a4,
    pliks_heard_voices_frequency   = awb2_11_pst_yr_3_a4,
    pliks_seen_things              = awb2_11_psychosis_5_r4,
    pliks_seen_things_distress     = awb2_11_upsetting_5_a4,
    pliks_seen_things_frequency    = awb2_11_pst_yr_5_a4,
    pliks_spied_on                 = awb2_11_psychosis_2_r4,
    pliks_spied_on_distress        = awb2_11_upsetting_2_a4,
    pliks_spied_on_frequency       = awb2_11_pst_yr_2_a4,
    pliks_spied_on_plot            = awb2_11_plot_hrm_a4,
    pliks_thoughts_read            = awb2_11_psychosis_10_r4,
    pliks_thoughts_read_distress   = awb2_11_upsetting_10_a4,
    pliks_thoughts_read_frequency  = awb2_11_pst_yr_10_a4,
    pliks_thoughts_read_powers     = awb2_11_pwrs_read_a4,
    pliks_tv_messages              = awb2_11_psychosis_1_r4,
    pliks_tv_messages_distress     = awb2_11_upsetting_1_a4,
    pliks_tv_messages_frequency    = awb2_11_pst_yr_1_a4,
    pliks_under_control            = awb2_11_psychosis_4_r4,
    pliks_under_control_distress   = awb2_11_upsetting_4_a4,
    pliks_under_control_frequency  = awb2_11_pst_yr_4_a4,
    pliks_under_control_who        = awb2_11_cntrl_who_a4,
    pliks_under_control_no_will    = awb2_11_cntrl_no_will_a4,
    pliks_special_powers           = awb2_11_psychosis_9_r4,
    pliks_special_powers_frequency = awb2_11_pst_yr_9_a4,
    
    # Demographics
    age_m_survey    = age_survey231_m, # Rename age at survey to same format as age at measurement variables
    age_y_survey    = age_survey_y, # Rename age at survey to same format as age at measurement variables
    survey_mode     = survey231_mode, # No need to specify module, both modules same mode for all participants
    sex             = awb1_2_sex,
    gender          = awb1_2_gender_r4,
    years_in_uk     = aw1_2_years_lvd_a4,
    live_where      = awb3_2_live_where_a10,
    imd_2019_decile = IMD_2019_decile,
    
    # Disability
    disability       = awb1_2_disability,
    disability_time  = awb1_2_disability_tme_a4,
    disability_limit = awb1_2_disability_impct_a4,
    
    # Assets
    assets_phone    = awb3_1_assets_4,
    assets_computer = awb3_1_assets_5,
    assets_holiday  = awb3_1_assets_6,
    assets_car      = awb3_1_assets_7,
    assets_bedroom  = awb3_1_assets_8,
    assets_compare  = awb3_1_compare_frnds,
    assets_worry    = awb3_1_money_wrry,
    assets_warmth   = awb3_1_warm_engh_a5,
    assets_save     = awb3_1_save_mny_a5,
    
    # Who do you live with?
    fam_live_where         = awb3_2_live_where_a10,  
    fam_lives_mother       = awb3_2_homes_1_ppl_r10___1,
    fam_lives_father       = awb3_2_homes_1_ppl_r10___2,
    fam_lives_guardian     = awb3_2_homes_1_ppl_r10___3,
    fam_lives_foster       = awb3_2_homes_1_ppl_r10___4,
    fam_lives_step_m       = awb3_2_homes_1_ppl_r10___5,
    fam_lives_step_f       = awb3_2_homes_1_ppl_r10___6,
    fam_lives_m_partner    = awb3_2_homes_1_ppl_r10___7,
    fam_lives_f_partner    = awb3_2_homes_1_ppl_r10___8,
    fam_lives_siblings     = awb3_2_homes_1_ppl_r10___9,
    fam_lives_auntie       = awb3_2_homes_1_ppl_r10___10,
    fam_lives_uncle        = awb3_2_homes_1_ppl_r10___11,
    fam_lives_grandmother  = awb3_2_homes_1_ppl_r10___12,
    fam_lives_grandfather  = awb3_2_homes_1_ppl_r10___13,
    fam_lives_cousins      = awb3_2_homes_1_ppl_r10___14,
    fam_lives_other        = awb3_2_homes_1_ppl_r10___15,
    fam_birth_order        = awb_2_6_family_brth_n_a5,
    fam_getalong_family    = awb2_6_family_rltnshp_1_a5,
    fam_getalong_siblings  = awb2_6_family_rltnshp_2_a5,
    
    # Personal assets
    assets_money_pocket   = awb3_4_personal_assts_1,
    assets_money_chores   = awb3_4_personal_assts_2,
    assets_money_business = awb3_4_personal_assts_3,
    assets_money_job      = awb3_4_personal_assts_4,
    assets_money_parents  = awb3_4_personal_assts_5,
    
    # Worries about food scarcity
    food_worry_1 = aw3_5_food_1,
    food_worry_2 = aw3_5_food_2,
    food_worry_3 = aw3_5_food_3,
    food_worry_4 = aw3_5_food_4,
    food_worry_5 = aw3_5_food_5,
    
    # Neighbourhood
    social_comparison  = aw3_6_comparison_2_r10,
    nbhd_safety_night  = awb3_7_prsnl_sfty_1,
    nbhd_safety_day    = awb3_7_prsnl_sfty_2,
    nbhd_safety_school = awb3_7_prsnl_sfty_4,
    nbhd_violence      = awb3_7_violence,
    
    # Activities
    activities_music     = awb3_activities_3_r10,
    activities_writing   = awb3_activities_15_r10,
    activities_volunteer = awb3_activities_11_r10,
    activities_art       = awb3_activities_17_r10,
    activities_visual    = awb3_activities_18_r10,
    activities_read      = awb3_activities_6_r10,
    activities_poetry    = awb3_activities_14_r10,
    activities_perform   = awb3_activities_16_r10,
    activities_party     = awb3_activities_1_r10,
    activities_museum    = awb3_activities_10_r10,
    activities_political = awb3_activities_12_r10,
    activities_religious = awb3_activities_13_r10,
    activities_gaming    = awb3_activities_19_a10,
    activities_spectate  = awb3_activities_2_r10,
    activities_concert   = awb3_activities_4_r10,
    activities_theatre   = awb3_activities_5_r10,
    activities_club      = awb3_activities_7_r10,
    activities_scouts    = awb3_activities_8_r10,
    activities_library   = awb3_activities_9_r10,
    
    # Local area events/pride
    area_proud           = awb3_pip_1_a10,
    area_events_proud    = awb3_pip_2_a10,
    area_events_interest = awb3_pip_3_a10,
    
    # General health, vision, hearing
    health_general = awb5_1_general_hlth,
    health_vision  = awb5_1_hearing_sght_1_r10,
    health_hearing = awb5_1_hearing_sght_3,
    
    # Food consumption
    food_breakfast        = awb5_1_food_dt_2,
    food_fruit            = awb5_1_food_dt_3,
    food_veg              = awb5_1_food_dt_4,
    food_drinks_sugarfree = awb5_1_food_dt_5,
    food_drinks_sugar     = awb5_1_food_dt_6_r7,
    food_drinks_energy    = awb5_1_food_dt_10_a10,
    food_fastfood         = awb5_1_food_dt_7,
    
    # Oral hygeine
    teeth_brush   = awb5_1_oral_hlth_1,
    teeth_dentist = awb5_1_oral_hlth_3,
    
    # Puerty
    puberty_changes     = awb4_4_puberty_a10,
    puberty_periods     = awb4_4y_gendersex_f_2,
    puberty_periods_age = awb4_4y_gendersex_y_f_2,
    
    # Drugs
    drugs_cigarettes         = awb5_1_cigs_a5,
    drugs_cigarettes_freq    = awb5_1_cigs2_r10,
    drugs_vape               = awb5_2_evr_vaped_a5,
    drugs_vape_behav         = awb5_2_vape_r10,
    drugs_alcohol            = awb5_2_alcohol,
    drugs_alcohol_age        = awb5_2y_alcohol_age,
    drugs_alcohol_freq       = awb5_2y_alcohol_frqncy,
    drugs_alcohol_binge      = awb5_2y_alcohol_qntty,
    drugs_alcohol_binge_age  = awb5_2_yalcohol_y_qntty_age,
    drugs_alcohol_binge_freq = awb5_2_yalcohol_y_qntty_frqncy,
    drugs_any                = awb5_drugs,
    drugs_cannibis           = awb5_2_drugs_1,
    drugs_ketamine           = awb5_2_drugs_10,
    drugs_spice              = awb5_2_drugs_12,
    drugs_nos                = awb5_2_drugs_15,
    drugs_presc              = awb5_2_drugs_17,
    drugs_cannibis_freq      = awb5_2_cannabis_pstyr,
    drugs_cocaine            = awb5_2_drugs_2,
    drugs_cocaine_freq       = awb5_2_cocaine_pstyr,
    drugs_ecstacy            = awb5_2_drugs_4,
    drugs_ecstacy_freq       = awb5_2_ecstasy_pstyr,
    drugs_ketamine_freq      = awb5_2_ket_pstyr,
    drugs_spice_freq         = awb5_2_spice_pstyr,
    drugs_nos_freq           = awb5_2_nitrous_pstyr,
    drugs_presc_freq         = awb5_2_prescription_pstyr,
    drugs_other              = awb5_2_drugs_othr_a10,
    drugs_other_freq         = awb5_2_drugs_othr3_a10,
    
    # Gambling
    gambling_lottery          = awb5_2_gambling_chk_a10___1,
    gambling_slot             = awb5_2_gambling_chk_a10___2,
    gambling_bet_private      = awb5_2_gambling_chk_a10___3,
    gambling_cards            = awb5_2_gambling_chk_a10___4,
    gambling_bingo_club       = awb5_2_gambling_chk_a10___5,
    gambling_bingo_other      = awb5_2_gambling_chk_a10___6,
    gambling_machine          = awb5_2_gambling_chk_a10___7,
    gambling_bet_shop         = awb5_2_gambling_chk_a10___8,
    gambling_casino           = awb5_2_gambling_chk_a10___9,
    gambling_online           = awb5_2_gambling_chk_a10___10,
    gambling_none             = awb5_2_gambling_chk_a10___0,
    gambling_lottery_when     = awb5_2_gambling_2_r10,
    gambling_slot_when        = awb5_2_gambling_6_r10,
    gambling_bet_private_when = awb5_2_gambling_7_r10,
    gambling_cards_when       = awb5_2_gambling_8_r10,
    gambling_bingo_club_when  = awb5_2_gambling_9_r10,
    gambling_bingo_other_when = awb5_2_gambling_10_r10,
    gambling_machine_when     = awb5_2_gambling_11_r10,
    gambling_bet_shop_when    = awb5_2_gambling_12_r10,
    gambling_casino_when      = awb5_2_gambling_13_r10,
    gambling_online_when      = awb5_2_gambling_14_r10,
    gambling_family           = awb5_2_gambling_fam_a10,
    
    # In-game purchases
    gaming_purchase_item   = awb5_2_online_gamb3_a5___1,
    gaming_purchase_unlock = awb5_2_online_gamb3_a5___2,
    gaming_purchase_bet    = awb5_2_online_gamb3_a5___3,
    gaming_purchase_none   = awb5_2_online_gamb3_a5___4,
    
    # Carried a knife or other weapon
    carried_weapon = awb5_2y_knife,
    
    # Encounters with police
    police_stop    = awb5_2_contactpolice_1,
    police_caution = awb5_2_contactpolice_2,
    
    # Physical activity (based on PAQ-A)
    pa_to_school    = awb4_1_physical_actvty_1_a5,
    pa_pe           = awb4_1_physical_actvty_2_a5,
    pa_lunch        = awb4_1_physical_actvty_3_a5,
    pa_break        = awb4_1_physical_actvty_4_a5,
    pa_from_school  = awb4_1_physical_actvty_5_a5,
    pa_after_school = awb4_1_physical_actvty_6_a5,
    pa_evening      = awb4_1_physical_actvty_7_a5,
    pa_weekend      = awb4_1_physical_actvty_8_a5,
    pa_sick         = awb4_1_sick_a5,
    
    # Sleep
    sleep_time_school = awb4_3_times2_a5,
    sleep_wake_school = awb4_3_times3_a5,
    sleep_time        = awb4_3_times4_a5,
    sleep_wake        = awb4_3_times5_a5,
    sleep_well        = awb4_3_sleep_well_nght_a5,
    sleepy_day        = awb4_3_sleep_drngday_a5,

    # Self Efficacy
    self_efficacy = awb2_3_self_effccy,
    
    # Trust
    trust = awb2_9_trust,
    
    # Self Harm
    self_harm = awb2_9_seek_hurt_self_a5,
    
    # Natural Environment
    env_greenspace_w = awb7_1_wntr_mnths,
    env_greenspace_s = awb7_1_smmr_mnths,
    env_air_quality  = awb7_2_pollution,
    env_future       = awb7_3_clmte_chnge_feelng,
    
    # School
    school_enjoy     = awb7_1_like,
    school_trust     = awb7_1_trust,
    school_bullying  = awb7_1_bullying_1,
    school_safe      = awb7_1_safe_r8,
    school_pressure1 = awb7_2_grades_a5,
    school_pressure2 = awb7_2_fam_diss_a5,
    school_pressure3 = awb7_2_prnt_preshr_a5,
    school_pressure4 = awb7_2_progrssng_a5,
    school_pressure5 = awb7_2_dnt_do_well_a5,
    school_pressure6 = awb7_2_acad_strss_r8,
    school_pressure7 = awb7_2_test_wrries_a5,
    
    # Friendships
    friends_offline = awb2_7_friends_physcl_r10,
    friends_online  = awb2_7_friends_onln_r10,
    friends_close   = awb2_7_friends_clse_a5,
    
    # Morality
    morality1 = awb8_1_morality_1_r10,
    morality2 = awb8_1_morality_2_r10,
    morality3 = awb8_1_morality_3_r10,
    morality4 = awb8_1_morality_4_r10,
    
    # Bullying
    bullying1 = awb8_3_bully,
    bullying2 = awb8_2_bullied,
    bullying3 = awb8_2_bullied_onlne,
    bullying4 = awb8_2_bully_onlne,
    
    # Discrimination
    discrim_discourage  = awb8_2_club_1,
    discrim_excluded    = awb8_2_excl_2,
    discrim_expected    = awb8_2_age_3,
    discrim_english     = awb8_2_lang_4,
    discrim_police      = awb8_2_police_5,
    discrim_staff       = awb8_2_shop_6,
    discrim_insults     = awb8_2_names_7,
    discrim_service     = awb8_2_service_8,
    discrim_intelligent = awb8_2_int_9,
    discrim_afraid      = awb8_2_afraid_10,
    discrim_threatened  = awb8_2_threat_11,
    
    #Social Media Use
    sm_use_Facebook  = awb6_1_social_media_r10___1,
    sm_use_Instagram = awb6_1_social_media_r10___2,
    sm_use_X         = awb6_1_social_media_r10___3,
    sm_use_TikTok    = awb6_1_social_media_r10___4,
    sm_use_Snapchat  = awb6_1_social_media_r10___5,
    sm_use_other     = awb6_1_social_media_r10___11,
    sm_time_weekday  = awb6_1_time_weekday,
    sm_time_weekend  = awb6_1_time_weekend,
    sm_positive1     = awb6_1_positive_exp___1,
    sm_positive2     = awb6_1_positive_exp___2,
    sm_positive3     = awb6_1_positive_exp___3,
    sm_positive4     = awb6_1_positive_exp___4,
    sm_positive5     = awb6_1_positive_exp___5,
    sm_positive6     = awb6_1_positive_exp___6,
    sm_negative1     = awb6_1_negative_exp_r5___1,
    sm_negative2     = awb6_1_negative_exp_r5___2,
    sm_negative3     = awb6_1_negative_exp_r5___3,
    sm_negative4     = awb6_1_negative_exp_r5___4,
    sm_negative5     = awb6_1_negative_exp_r5___5,
    sm_negative6     = awb6_1_negative_exp_r5___6,
    sm_negative7     = awb6_1_negative_exp_r5___7,
    sm_experience    = awb6_1_pos_neg,
    
    # Internet
    internet_access  = awb6_6_int_hme,
    internet_quality = awb6_6_int_hme_gd,
    
    # Bioimpedance
    clothes_weight = clothes
  )
    

# 6. Reduce categorical variables ####

## Birth place ####

# Replace "----------------" with NA
aow_curated$awb1_2_country_brth[aow_curated$awb1_2_country_brth == 202] <- NA

# Drop unused value labels
aow_curated$awb1_2_country_brth <- drop_unused_value_labels(aow_curated$awb1_2_country_brth)

aow_curated$fam_birth_place <- case_match(aow_curated$awb1_2_country_brth,
                                     c(1:4, 190) ~ "UK",
                                     NA ~ NA,
                                     .default = "Other")

## Ethnicity ####
aow_curated$ethnicity <- case_match(aow_curated$awb1_2_ethnicity_r4,
                                    3 ~ "Asian or Asian British",
                                    1 ~ "White",
                                    c(2,4:6) ~ "Other",
                                    NA ~ NA)

## Language spoken at home ####

# Is English and/or other language spoken at home?

aow_curated$lang_home <- ""
aow_curated <- aow_curated %>%
   mutate(other_languages = rowSums(select(., awb1_2_language_hme___2:awb1_2_language_hme___9)) > 0,
          lang_home = case_when(
            awb1_2_language_hme___1 == 1 & other_languages == TRUE ~ "English and other(s)",
            awb1_2_language_hme___1 == 1 & other_languages == FALSE ~ "English only",
            awb1_2_language_hme___1 == 0 & other_languages == TRUE ~ "Language(s) other than English",
            TRUE ~ NA_character_ # default case for any other scenario
          )) %>%
   select(-other_languages) # Remove the temporary variable

# How many languages are spoken at home?

aow_curated <- aow_curated %>%
  mutate(lang_number = rowSums(select(., starts_with("awb1_2_language_hme"))))
aow_curated$lang_number[aow_curated$lang_number == 0] <- NA
# Reduce to 1, 2, 3 or more
aow_curated$lang_number <- case_match(aow_curated$lang_number,
                                      1 ~ "1",
                                      2 ~ "2",
                                      c(3:9) ~ "3 or more",
                                      NA ~ NA)

## Religion ####

# First rename the religion variables for transparency
aow_curated <- aow_curated %>%
  rename(has_religion = awb1_2_religion) %>%
  rename(religion = awb1_2y_religion_r4)

# Reduce religion categories
aow_curated$religion <- case_match(aow_curated$religion,
                                   1 ~ "Christianity",
                                   5 ~ "Islam",
                                   c(2:4, 6:7) ~ "Other/Not specified",
                                   NA ~ NA)

# For those who responded "Yes" to has_religion, but NA for religion, set religion to "Other/Not specified"
aow_curated <- aow_curated %>%
  mutate(religion = case_when(
    has_religion == 1 & is.na(religion) ~ "Other/Not specified",
    TRUE ~ religion
  ))

# For those who responded "No" to has_religion, set religion to "None"
aow_curated <- aow_curated %>%
  mutate(religion = case_when(
    has_religion == 2 ~ "None",
    TRUE ~ religion
  ))

## Disability ####

# Re-label disability_time values for transparency
aow_curated$disability_time <- case_match(aow_curated$disability_time,
                                    1 ~ "One year or more",
                                    2 ~ "Less than a year",
                                    NA ~ NA)

## discouraged from joining a club ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_club_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_discourage_reason = case_when(
      
      # If the main question 'discrim_discourage' is 0 ('No'), the reason is not applicable.
      discrim_discourage == 0 ~ 'discrim_discourage answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_discourage == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_club_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_club_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_club_rsn_1___3 == 1 ~ "Disability",
      awb8_2_club_rsn_1___4 == 1 ~ "Religion",
      awb8_2_club_rsn_1___5 == 1 ~ "Class",
      awb8_2_club_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_club_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_club_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_discourage answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Excluded from activities ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_excl_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_excluded_reason = case_when(
      
      # If the main question 'discrim_excluded' is 0 ('No'), the reason is not applicable.
      discrim_excluded == 0 ~ 'discrim_excluded answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_excluded == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_excl_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_excl_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_excl_rsn_1___3 == 1 ~ "Disability",
      awb8_2_excl_rsn_1___4 == 1 ~ "Religion",
      awb8_2_excl_rsn_1___5 == 1 ~ "Class",
      awb8_2_excl_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_excl_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_excl_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_excluded answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Expected less of than others your age ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_age_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_expected_reason = case_when(
      
      # If the main question 'discrim_expected' is 0 ('No'), the reason is not applicable.
      discrim_expected == 0 ~ 'discrim_expected answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_expected == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_age_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_age_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_age_rsn_1___3 == 1 ~ "Disability",
      awb8_2_age_rsn_1___4 == 1 ~ "Religion",
      awb8_2_age_rsn_1___5 == 1 ~ "Class",
      awb8_2_age_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_age_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_age_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_expected answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Have people assumed your English was poor ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_lang_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_english_reason = case_when(
      
      # If the main question 'discrim_english' is 0 ('No'), the reason is not applicable.
      discrim_english == 0 ~ 'discrim_english answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_english == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_lang_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_lang_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_lang_rsn_1___3 == 1 ~ "Disability",
      awb8_2_lang_rsn_1___4 == 1 ~ "Religion",
      awb8_2_lang_rsn_1___5 == 1 ~ "Class",
      awb8_2_lang_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_lang_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_lang_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_english answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Have you ever been hassled by police ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_police_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_police_reason = case_when(
      
      # If the main question 'discrim_police' is 0 ('No'), the reason is not applicable.
      discrim_police == 0 ~ 'discrim_police answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_police == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_police_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_police_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_police_rsn_1___3 == 1 ~ "Disability",
      awb8_2_police_rsn_1___4 == 1 ~ "Religion",
      awb8_2_police_rsn_1___5 == 1 ~ "Class",
      awb8_2_police_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_police_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_police_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_police answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Have you ever been hassled by staff in a shop ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_shop_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_staff_reason = case_when(
      
      # If the main question 'discrim_staff' is 0 ('No'), the reason is not applicable.
      discrim_staff == 0 ~ 'discrim_staff answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_staff == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_shop_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_shop_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_shop_rsn_1___3 == 1 ~ "Disability",
      awb8_2_shop_rsn_1___4 == 1 ~ "Religion",
      awb8_2_shop_rsn_1___5 == 1 ~ "Class",
      awb8_2_shop_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_shop_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_shop_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_staff answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Have you ever been called insulting names ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_names_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_insults_reason = case_when(
      
      # If the main question 'discrim_insults' is 0 ('No'), the reason is not applicable.
      discrim_insults == 0 ~ 'discrim_insults answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_insults == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_names_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_names_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_names_rsn_1___3 == 1 ~ "Disability",
      awb8_2_names_rsn_1___4 == 1 ~ "Religion",
      awb8_2_names_rsn_1___5 == 1 ~ "Class",
      awb8_2_names_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_names_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_names_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_insults answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Have you ever received poor service in a shop/restaurant ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_service_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_service_reason = case_when(
      
      # If the main question 'discrim_service' is 0 ('No'), the reason is not applicable.
      discrim_service == 0 ~ 'discrim_service answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_service == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_service_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_service_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_service_rsn_1___3 == 1 ~ "Disability",
      awb8_2_service_rsn_1___4 == 1 ~ "Religion",
      awb8_2_service_rsn_1___5 == 1 ~ "Class",
      awb8_2_service_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_service_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_service_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_service answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Have people ever acted as though you were not intelligent ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_int_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_intelligent_reason = case_when(
      
      # If the main question 'discrim_intelligent' is 0 ('No'), the reason is not applicable.
      discrim_intelligent == 0 ~ 'discrim_intelligent answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_intelligent == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_int_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_int_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_int_rsn_1___3 == 1 ~ "Disability",
      awb8_2_int_rsn_1___4 == 1 ~ "Religion",
      awb8_2_int_rsn_1___5 == 1 ~ "Class",
      awb8_2_int_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_int_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_int_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_intelligent answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Have people ever acted like they're afraid of you ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_afraid_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_afraid_reason = case_when(
      
      # If the main question 'discrim_afraid' is 0 ('No'), the reason is not applicable.
      discrim_afraid == 0 ~ 'discrim_afraid answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_afraid == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_afraid_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_afraid_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_afraid_rsn_1___3 == 1 ~ "Disability",
      awb8_2_afraid_rsn_1___4 == 1 ~ "Religion",
      awb8_2_afraid_rsn_1___5 == 1 ~ "Class",
      awb8_2_afraid_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_afraid_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_afraid_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_afraid answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Have you ever been threatened ####

# Define the names of your eight follow-up reason variables
reason_vars <- paste0("awb8_2_threat_rsn_1___", 1:8)

# process the data frame
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary helper column to count how many reasons were checked per row
    reason_count = rowSums(across(all_of(reason_vars))),
    
    # Create the new categorical variable using conditional logic
    discrim_threatened_reason = case_when(
      
      # If the main question 'discrim_threatened' is 0 ('No'), the reason is not applicable.
      discrim_threatened == 0 ~ 'discrim_threatened answer == 0',
      
      # If the main question was 'Yes' but no reason was checked.
      discrim_threatened == 1 & reason_count == 0 ~ "No reason given",
      
      # If more than one reason was checked.
      reason_count == 2 ~ "Two reasons given",
      reason_count > 2 ~ "Three or more reasons given",
      
      # If exactly one reason was checked, assign the category based on which box was ticked.
      # NOTE: Replace "Reason 1", "Reason 2", etc., with the actual meanings if you know them.
      awb8_2_threat_rsn_1___1 == 1 ~ "Ethnicity",
      awb8_2_threat_rsn_1___2 == 1 ~ "Sex/Gender",
      awb8_2_threat_rsn_1___3 == 1 ~ "Disability",
      awb8_2_threat_rsn_1___4 == 1 ~ "Religion",
      awb8_2_threat_rsn_1___5 == 1 ~ "Class",
      awb8_2_threat_rsn_1___6 == 1 ~ "Neurodiversity",
      awb8_2_threat_rsn_1___7 == 1 ~ "Sexuality",
      awb8_2_threat_rsn_1___8 == 1 ~ "Other",
      
      # A fallback for any unexpected cases.
      TRUE ~ 'discrim_threatened answer missing'
    )
  ) %>%
  # Finally, remove the temporary helper column as it's no longer needed
  select(-reason_count)

## Remove old and now redundant variables ####
aow_curated <- aow_curated %>%
  select(
    # replaced by ethnicity variable
    -awb1_2_ethnicity_r4,
    
    # replaced by new binary fam_birth_place variable
    -awb1_2_country_brth,
    
    # replaced by lang_home and lang_number variables
    -awb1_2_language_hme___1,
    -awb1_2_language_hme___2,
    -awb1_2_language_hme___3,
    -awb1_2_language_hme___4,
    -awb1_2_language_hme___5,
    -awb1_2_language_hme___6,
    -awb1_2_language_hme___7,
    -awb1_2_language_hme___8,
    -awb1_2_language_hme___9, 
    
    # This information is in religion variable
    -has_religion, 
    
    # replaced by categorical variable 'discouraged_club_reason'
    -awb8_2_club_rsn_1___1, 
    -awb8_2_club_rsn_1___2,
    -awb8_2_club_rsn_1___3,
    -awb8_2_club_rsn_1___4,
    -awb8_2_club_rsn_1___5,
    -awb8_2_club_rsn_1___6,
    -awb8_2_club_rsn_1___7,
    -awb8_2_club_rsn_1___8,
    
    # replaced by categorical variable 'excluded_activities_reason'
    -awb8_2_excl_rsn_1___1,
    -awb8_2_excl_rsn_1___2,
    -awb8_2_excl_rsn_1___3,
    -awb8_2_excl_rsn_1___4,
    -awb8_2_excl_rsn_1___5,
    -awb8_2_excl_rsn_1___6,
    -awb8_2_excl_rsn_1___7,
    -awb8_2_excl_rsn_1___8,
    
    # replaced by categorical variable 'expected_less_reason'
    -awb8_2_age_rsn_1___1, 
    -awb8_2_age_rsn_1___2,
    -awb8_2_age_rsn_1___3,
    -awb8_2_age_rsn_1___4,
    -awb8_2_age_rsn_1___5,
    -awb8_2_age_rsn_1___6,
    -awb8_2_age_rsn_1___7,
    -awb8_2_age_rsn_1___8,
    
    # replaced by categorical variable 'assumed_english_reason'
    -awb8_2_lang_rsn_1___1, 
    -awb8_2_lang_rsn_1___2,
    -awb8_2_lang_rsn_1___3,
    -awb8_2_lang_rsn_1___4,
    -awb8_2_lang_rsn_1___5,
    -awb8_2_lang_rsn_1___6,
    -awb8_2_lang_rsn_1___7,
    -awb8_2_lang_rsn_1___8,
    
    # replaced by categorical variable 'hassled_police_reason'
    -awb8_2_police_rsn_1___1, 
    -awb8_2_police_rsn_1___2,
    -awb8_2_police_rsn_1___3,
    -awb8_2_police_rsn_1___4,
    -awb8_2_police_rsn_1___5,
    -awb8_2_police_rsn_1___6,
    -awb8_2_police_rsn_1___7,
    -awb8_2_police_rsn_1___8, 
    
    # replaced by categorical variable 'hassled_police_reason'
    -awb8_2_shop_rsn_1___1, 
    -awb8_2_shop_rsn_1___2,
    -awb8_2_shop_rsn_1___3,
    -awb8_2_shop_rsn_1___4,
    -awb8_2_shop_rsn_1___5,
    -awb8_2_shop_rsn_1___6,
    -awb8_2_shop_rsn_1___7,
    -awb8_2_shop_rsn_1___8,
    
    # replaced by categorical variable 'insulting_names_reason'
    -awb8_2_names_rsn_1___1,
    -awb8_2_names_rsn_1___2,
    -awb8_2_names_rsn_1___3,
    -awb8_2_names_rsn_1___4,
    -awb8_2_names_rsn_1___5,
    -awb8_2_names_rsn_1___6,
    -awb8_2_names_rsn_1___7,
    -awb8_2_names_rsn_1___8,
    
    # replaced by categorical variable 'poor_service_reason'
    -awb8_2_service_rsn_1___1,
    -awb8_2_service_rsn_1___2,
    -awb8_2_service_rsn_1___3,
    -awb8_2_service_rsn_1___4,
    -awb8_2_service_rsn_1___5,
    -awb8_2_service_rsn_1___6,
    -awb8_2_service_rsn_1___7,
    -awb8_2_service_rsn_1___8,
    
    # replaced by categorical variable 'act_intelligent_reason'
    -awb8_2_int_rsn_1___1,
    -awb8_2_int_rsn_1___2,
    -awb8_2_int_rsn_1___3,
    -awb8_2_int_rsn_1___4,
    -awb8_2_int_rsn_1___5,
    -awb8_2_int_rsn_1___6,
    -awb8_2_int_rsn_1___7,
    -awb8_2_int_rsn_1___8,
    
    # replaced by categorical variable 'act_afraid_reason'
    -awb8_2_afraid_rsn_1___1,
    -awb8_2_afraid_rsn_1___2,
    -awb8_2_afraid_rsn_1___3,
    -awb8_2_afraid_rsn_1___4,
    -awb8_2_afraid_rsn_1___5,
    -awb8_2_afraid_rsn_1___6,
    -awb8_2_afraid_rsn_1___7,
    -awb8_2_afraid_rsn_1___8,
    
    # replaced by categorical variable 'been_threatened_reason'
    -awb8_2_threat_rsn_1___1,
    -awb8_2_threat_rsn_1___2,
    -awb8_2_threat_rsn_1___3,
    -awb8_2_threat_rsn_1___4,
    -awb8_2_threat_rsn_1___5,
    -awb8_2_threat_rsn_1___6,
    -awb8_2_threat_rsn_1___7,
    -awb8_2_threat_rsn_1___8     
  )

# 7. Find missing responses from checked vs un-checked variable responses ####

# Gambling questions

# Define the variable names
gambling_vars <- c(
  "gambling_lottery",
  "gambling_slot",
  "gambling_bet_private",
  "gambling_cards",
  "gambling_bingo_club",
  "gambling_bingo_other",
  "gambling_machine",
  "gambling_bet_shop",
  "gambling_casino",
  "gambling_online",
  "gambling_none"
)

# Reclassify the variables 
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary logical condition for each row
    all_are_zeros = rowSums(select(., all_of(gambling_vars))) == 0,
    
    # Apply the change across all specified variables
    across(all_of(gambling_vars), 
           ~ if_else(all_are_zeros, NA, .))
  ) %>%
  
  # Remove the temporary column
  select(-all_are_zeros)

# Online games questions

# Define the variable names
online_gaming_vars <- c(
  'gaming_purchase_item',
  'gaming_purchase_unlock',
  'gaming_purchase_bet',
  'gaming_purchase_none'
)

# Reclassify the variables 
aow_curated <- aow_curated %>%
  mutate(
    # Create a temporary logical condition for each row
    all_are_zeros = rowSums(select(., all_of(online_gaming_vars))) == 0,
    
    # Apply the change across all specified variables
    across(all_of(online_gaming_vars), 
           ~ if_else(all_are_zeros, NA, .))
  ) %>%
  
  # Remove the temporary column
  select(-all_are_zeros)

# 8. Divide data into year groups ####

aow_year_8 <- aow_curated %>% 
  filter(year_group == 8)

aow_year_9 <- aow_curated %>% 
  filter(year_group == 9)

aow_year_10 <- aow_curated %>% 
  filter(year_group == 10)

# 9. Save the processed data ####

# Save as a CSV file (Comma-Separated Values)
# Best for universal access with any spreadsheet or coding software.
write_csv(aow_curated, here("data", "derived", "aow_curated.csv"))

# Save as an R Data file (.rds)
# Best for saving and reloading into R. Perfectly preserves data types.
saveRDS(aow_curated, here("data", "derived", "aow_curated.rds"))

# Save as a Stata file (.dta)
# For users of the Stata statistical software.
write_dta(aow_curated, here("data", "derived", "aow_curated.dta"))

# Save as an SPSS file (.sav)
# For users of the SPSS statistical software.
write_sav(aow_curated, here("data", "derived", "aow_curated.sav"))

# Confirmation message
print(
  "The script has finished processing the raw data. Processed data saved in CSV, RDS, Stata, and SPSS formats can be found in the 'data/derived' folder."
)