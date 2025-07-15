# Check number of unique IDs
length(unique(as.factor(recruitment_dr23$aow_person_id)))

# Variable info
str(recruitment_dr24)
sapply(recruitment_dr23, class)

#### Get list of variables and variable types as csv for a df ####
# Initialize empty vectors to store names and types
variable_names <- character(0)
variable_types <- character(0)
# Loop through each column of the data frame
for (col_name in names(recruitment_dr24)) {
  variable_names <- c(variable_names, col_name)
  # Get the class of the column. Use `[1]` to take only the primary class
  # if a vector has multiple classes (e.g., "haven_labelled").
  current_class <- class(recruitment_dr24[[col_name]])[1] 
  variable_types <- c(variable_types, current_class)
}
# Create the data frame
recruitment_dr24_info <- data.frame(
  Variable_name = variable_names,
  Variable_type = variable_types
)
# Save csv
write.csv(recruitment_dr24_info, "recruitment_dr24_info.csv", row.names = FALSE)


