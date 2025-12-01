# Check number of unique IDs
length(unique(as.factor(dataframe$variable))) #replace 'dataframe' and 'variable' with your dataframe and variable name

# Variable info
str(dataframe) #replace 'dataframe' with your dataframe name
sapply(dataframe, class) #replace 'dataframe' with your dataframe name

#### Get list of variables, variable types, and labels as csv for a df ####
# Initialize empty vectors to store names, types, and labels
variable_names <- character(0)
variable_types <- character(0)
variable_labels <- character(0)

# Loop through each column of the data frame
for (col_name in names(dataframe)) { #replace 'dataframe' with your dataframe name
  # Add variable name
  variable_names <- c(variable_names, col_name)
  
  # Get the class of the column. Use `[1]` to take only the primary class
  # if a vector has multiple classes (e.g., "haven_labelled").
  current_class <- class(dataframe[[col_name]])[1] #replace 'dataframe' with your dataframe name
  variable_types <- c(variable_types, current_class)
  
  # Extract variable label
  current_label <- attr(dataframe[[col_name]], "label") #replace 'dataframe' with your dataframe name
  
  # Handle cases where no label attribute exists
  if (is.null(current_label)) {
    variable_labels <- c(variable_labels, "") # If no label, add an empty string
  } else {
    variable_labels <- c(variable_labels, current_label) # Add the found label
  }
}

# Create the data frame
dataframe <- data.frame( #replace 'dataframe' with your dataframe name
  Variable_name = variable_names,
  Variable_type = variable_types,
  Variable_label = variable_labels, # Add the new label column
  stringsAsFactors = FALSE # Important for character columns
)

# Save csv
write.csv(dataframe, "dataframe_info.csv", row.names = FALSE) #replace 'dataframe' with your dataframe name

# Delete object to tidy environment
rm(dataframe_info)


##### Merge Survey Module Dataframes ####
library(dplyr)

mergeddataframe <- full_join(dataframe1, dataframe2, #replace with your dataframe names where appropriate
                    by = "aow_recruitment_id",
                    keep = TRUE)

# re: "Note: Data were not all collected at the same time, see differences in age
# (months) between measurements
table(dataframe$age_survey231_m, dataframe$age_m_heightweight) #replace 'dataframe' with your dataframe name
table(dataframe$age_survey231_m, dataframe$age_m_skinfold)
table(dataframe$age_survey231_m, dataframe$age_m_bloodpressure)
table(dataframe$age_survey231_m, dataframe$age_m_bioimpedance)

## Summary of scale total scores ####
 
summary(dataframe$edeqs_total) #replace 'dataframe' with your dataframe name
summary(dataframe$rcads_anx_total)
summary(dataframe$rcads_dep_total)
summary(dataframe$rcads_total)
summary(dataframe$swemwbs_total)
summary(dataframe$sdq_con_total)
summary(dataframe$sdq_emo_total)
summary(dataframe$sdq_hyp_total)
summary(dataframe$sdq_pee_total)
summary(dataframe$sdq_pro_total)
summary(dataframe$sdq_int_total)
summary(dataframe$sdq_ext_total)
summary(dataframe$sdq_dif_total)
summary(dataframe$ucla3_total)
summary(dataframe$ucla4_total)
summary(dataframe$brs_total)
summary(dataframe$yaps_total)

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