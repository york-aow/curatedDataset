#### Functions ####

#### Get dataframe variable info as a dataframe + save as CSV ####

df_info <- function(df, file = NULL) {
  # Initialize empty vectors to store names, types, and labels
  variable_names <- character(0)
  variable_types <- character(0)
  variable_labels <- character(0)
  
  # Loop through each column of the data frame
  for (col_name in names(df)) {
    # Add variable name
    variable_names <- c(variable_names, col_name)
    
    # Get the class of the column. Use `[1]` to take only the primary class
    # if a vector has multiple classes (e.g., "haven_labelled").
    current_class <- class(df[[col_name]])[1]
    variable_types <- c(variable_types, current_class)
    
    # Extract variable label
    # Try to get the "label" attribute, which is common for data imported
    # from statistical software (e.g., using 'haven' package).
    current_label <- attr(df[[col_name]], "label")
    
    # Handle cases where no label attribute exists
    if (is.null(current_label)) {
      variable_labels <- c(variable_labels, "") # If no label, add an empty string
    } else {
      variable_labels <- c(variable_labels, current_label) # Add the found label
    }
  }
  
  # Create the data frame containing the extracted information
  df_info <- data.frame(
    Variable_name = variable_names,
    Variable_type = variable_types,
    Variable_label = variable_labels, # Add the new label column
    stringsAsFactors = FALSE # For character columns to remain as characters
  )
  
  # Save csv if a file path is provided (i.e., not NULL)
  if (!is.null(file)) {
    tryCatch({
      write.csv(df_info, file, row.names = FALSE)
      message(paste("Dataframe info successfully saved to:", file))
    }, error = function(e) {
      warning(paste("Failed to save CSV to", file, ":", e$message))
    })
  }
}