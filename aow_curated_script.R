# 0. Install and load required packages ####

install.packages("tidyverse") # tidy R packages for data manipulation etc
install.packages("haven") # Import data of various formats
install.packages("psych") # Function to pro-rate missing items (item.scores)

library(tidyverse) # tidy R packages for data manipulation etc
library(haven) # Import data of various formats
library(psych) # Function to pro-rate missing items (item.scores)


# 1. Load raw AoW data ####

# a. Set the source folder path
source_folder <- 'G:/Shared drives/Curated Dataset/data/source' # Paste the filepath here. would be good to have path set automatically e.g. using 'getwd'?

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
# between measurements
table(aow_curated$age_survey231_m, aow_curated$age_m_heightweight)
table(aow_curated$age_survey231_m, aow_curated$age_m_skinfold)
table(aow_curated$age_survey231_m, aow_curated$age_m_bloodpressure)
table(aow_curated$age_survey231_m, aow_curated$age_m_bioimpedance)

# 3. Remove unnecessary variables ####

aow_curated <- aow_curated %>% 
  select(
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
    -has_measure, # unnecessary
    -survey231_version, # all version 10
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
    -age_y, # age in months (age_m) more useful
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
  mutate(awb2_12_eat_hbt_10_a5 = ifelse(awb2_12_eat_hbt_9_a5 == 0, 0, awb2_12_eat_hbt_10_a5))

# Identify complete EDE-QS responses
aow_curated <- aow_curated %>%
  mutate(edeqs_complete = rowSums(is.na(select(., !!!edeqs_items))) == 0) # !!! removes quote marks from the list items

# Compute total scores for those with complete EDE-QS, and assign NA to those without complete edeqs
aow_curated <- aow_curated %>%
  mutate(edeqs_total = ifelse(edeqs_complete, rowSums(select(., !!!edeqs_items), na.rm = TRUE), NA))

## RCADS ####

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

# Create list of all RCADS items
rcads_items <- c(rcads_anx_items, rcads_dep_items)

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


## Summary ####

summary(aow_curated$edeqs_total)
summary(aow_curated$rcads_anx_total)
summary(aow_curated$rcads_dep_total)
summary(aow_curated$rcads_total)
summary(aow_curated$swemwbs_total)
summary(aow_curated$sdq_con_total)
summary(aow_curated$sdq_emo_total)
summary(aow_curated$sdq_hyp_total)
summary(aow_curated$sdq_pee_total)
summary(aow_curated$sdq_pro_total)
summary(aow_curated$sdq_int_total)
summary(aow_curated$sdq_ext_total)
summary(aow_curated$sdq_dif_total)
summary(aow_curated$ucla3_total)
summary(aow_curated$ucla4_total)
summary(aow_curated$brs_total)
summary(aow_curated$yaps_total)


# 5. Divide data into year groups ####

year_8 <- aow_curated %>% 
  filter(year_group == 8)

year_9 <- aow_curated %>% 
  filter(year_group == 9)

year_10 <- aow_curated %>% 
  filter(year_group == 10)



# Rename variables ####

# Put RCADS item list in right order
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

# Put SDQ items list in right order
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

# Create new variable names
rcads_items_new <- paste("rcads_item", seq_along(rcads_items), sep = "_")
sdq_items_new <- paste("sdq_item", seq_along(sdq_items), sep = "_")
edeqs_items_new <- paste("edeqs_item", seq_along(edeqs_items), sep = "_")
swemwbs_items_new <- paste("swemwbs_item", seq_along(swemwbs_items), sep = "_")
brs_items_new <- paste("brs_item", seq_along(brs_items), sep = "_")
yaps_items_new <- paste("yaps_item", seq_along(yaps_items), sep = "_")

# Rename the variables
aow_curated <- aow_curated %>%
  rename(!!!setNames(rcads_items, rcads_items_new)) %>%
  rename(!!!setNames(sdq_items, sdq_items_new)) %>%
  rename(!!!setNames(edeqs_items, edeqs_items_new)) %>%
  rename(!!!setNames(swemwbs_items, swemwbs_items_new)) %>%
  rename(!!!setNames(brs_items, brs_items_new)) %>%
  rename(!!!setNames(yaps_items, yaps_items_new))


df_info(aow_curated, file = "aow_curated_2.csv") # doesn't work without the function script. save to output folder??

