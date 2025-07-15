# Check number of unique IDs
length(unique(as.factor(recruitment_dr23$aow_person_id)))

# Variable info
str(recruitment_dr24)
sapply(recruitment_dr23, class)

#### Get list of df variables, variable types, and labels as csv ####
# Initialize empty vectors to store names, types, and labels
variable_names <- character(0)
variable_types <- character(0)
variable_labels <- character(0)

# Loop through each column of the data frame
for (col_name in names(survey_mod231_derived_dr24)) {
  # Add variable name
  variable_names <- c(variable_names, col_name)
  
  # Get the class of the column. Use `[1]` to take only the primary class
  # if a vector has multiple classes (e.g., "haven_labelled").
  current_class <- class(survey_mod231_derived_dr24[[col_name]])[1]
  variable_types <- c(variable_types, current_class)
  
  # Extract variable label
  current_label <- attr(survey_mod231_derived_dr24[[col_name]], "label") # Try to get the "label" attribute
  
  # Handle cases where no label attribute exists
  if (is.null(current_label)) {
    variable_labels <- c(variable_labels, "") # If no label, add an empty string
  } else {
    variable_labels <- c(variable_labels, current_label) # Add the found label
  }
}

# Create the data frame
survey_mod231_derived_dr24_info <- data.frame(
  Variable_name = variable_names,
  Variable_type = variable_types,
  Variable_label = variable_labels, # Add the new label column
  stringsAsFactors = FALSE # Important for character columns
)

# Save csv
write.csv(survey_mod231_derived_dr24_info, "survey_mod231_derived_dr24_info.csv", row.names = FALSE)

# Delete object to tidy environment
rm(survey_mod231_derived_dr24_info)
