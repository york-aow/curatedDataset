# Check number of unique IDs
length(unique(as.factor(survey_mod231_main_dr24$aow_person_id)))
length(unique(as.factor(survey_mod232_main_dr24$aow_person_id)))
length(unique(as.factor(survey_mod231_main_dr24$aow_recruitment_id)))
length(unique(as.factor(survey_mod232_main_dr24$aow_recruitment_id)))

# Variable info
str(recruitment_dr24)
sapply(survey_mod231_main_dr24, class)

#### Get list of variables, variable types, and labels as csv for a df ####
# Initialize empty vectors to store names, types, and labels
variable_names <- character(0)
variable_types <- character(0)
variable_labels <- character(0)

# Loop through each column of the data frame
for (col_name in names(survey_mod232_derived_dr24)) {
  # Add variable name
  variable_names <- c(variable_names, col_name)
  
  # Get the class of the column. Use `[1]` to take only the primary class
  # if a vector has multiple classes (e.g., "haven_labelled").
  current_class <- class(survey_mod232_derived_dr24[[col_name]])[1]
  variable_types <- c(variable_types, current_class)
  
  # Extract variable label
  current_label <- attr(survey_mod232_derived_dr24[[col_name]], "label") # Try to get the "label" attribute
  
  # Handle cases where no label attribute exists
  if (is.null(current_label)) {
    variable_labels <- c(variable_labels, "") # If no label, add an empty string
  } else {
    variable_labels <- c(variable_labels, current_label) # Add the found label
  }
}

# Create the data frame
survey_mod232_derived_dr24_info <- data.frame(
  Variable_name = variable_names,
  Variable_type = variable_types,
  Variable_label = variable_labels, # Add the new label column
  stringsAsFactors = FALSE # Important for character columns
)

# Save csv
write.csv(survey_mod232_derived_dr24_info, "survey_mod232_derived_dr24_info.csv", row.names = FALSE)

# Delete object to tidy environment
rm(survey_mod232_derived_dr24_info)


##### Merge Survey Module Dataframes ####
library(dplyr)

merge2 <- full_join(survey_mod231_main_dr24, survey_mod232_main_dr24, 
                    by = "aow_recruitment_id",
                    keep = TRUE)

# re: "Note: Data were not all collected at the same time, see differences in age
# (months) between measurements
table(aow_curated$age_survey231_m, aow_curated$age_m_heightweight)
table(aow_curated$age_survey231_m, aow_curated$age_m_skinfold)
table(aow_curated$age_survey231_m, aow_curated$age_m_bloodpressure)
table(aow_curated$age_survey231_m, aow_curated$age_m_bioimpedance)

## Summary of scale total scores ####

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

# birth place labels/tables
print_labels(aow_curated$awb1_2_country_brth)
table(aow_curated$awb1_2_country_brth)
table(as_factor(aow_curated$awb1_2_country_brth), useNA = "ifany")
table(aow_curated$fam_birth_place, useNA = "ifany")

# ethnicity labels/tables
print_labels(aow_curated$awb1_2_ethnicity_r4)
table(as_factor(aow_curated$awb1_2_ethnicity_r4), useNA = "ifany")
table(as_factor(aow_curated$ethnicity), useNA = "ifany")

#languages labels/tables
table(aow_curated$lang_home, useNA = "ifany")
table(aow_curated$lang_number, useNA = "ifany")

#religion labels/tables
table(as_factor(aow_curated$religion),as_factor(aow_curated$has_religion), useNA = "ifany")
print_labels(aow_curated$has_religion) # 1 = Yes, 2 = No
print_labels(aow_curated$religion)
table(as_factor(aow_curated$religion),as_factor(aow_curated$has_religion), useNA = "ifany")
table(as_factor(aow_curated$religion),as_factor(aow_curated$has_religion), useNA = "ifany")
table(as_factor(aow_curated$has_religion), useNA = "ifany")
table(as_factor(aow_curated$religion),as_factor(aow_curated$has_religion), useNA = "ifany")
table(as_factor(aow_curated$religion), useNA = "ifany")

#disability labels/tables
print_labels(aow_curated$disability_time)
table(as_factor(aow_curated$disability_time))