#### Get list of variables and variable types as csv for a df ####
# Initialize empty vectors to store names and types
variable_names <- character(0)
variable_types <- character(0)
variable_labels <- character(0)

# Loop through each column of the data frame
for (col_name in names(survey_mod04_dr23)) {
  variable_names <- c(variable_names, col_name)
  
  # Get the class of the column. Use `[1]` to take only the primary class
  # if a vector has multiple classes (e.g., "numeric", "integer").
  current_class <- class(survey_mod04_dr23[[col_name]])[1] 
  variable_types <- c(variable_types, current_class)
  
  # Get the label attribute
  current_label <- attr(survey_mod04_dr23[[col_name]], "label")
  
  # If no label exists, use the variable name as a fallback or an empty string
  if (is.null(current_label)) {
    variable_labels <- c(variable_labels, "") # Or col_name if you prefer
  } else {
    variable_labels <- c(variable_labels, current_label)
  }
}

# Create the data frame
survey_mod04_dr23_info <- data.frame(
  Variable_name = variable_names,
  Variable_type = variable_types,
  Variable_label = variable_labels
)
# Save csv
write.csv(survey_mod04_dr23_info, "survey_mod04_dr23_info1.csv", row.names = FALSE)

