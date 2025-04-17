# scripts/sep2_prototype.R
library(data.table)

# Helper function containing the actual splitting logic
apply_sep2_splitting <- function(dt, sep_char, cols_split) {

  dt_copy <- copy(dt)
  for(col in cols_split) {
    if (!col %in% names(dt_copy)) {
      warning(paste("Column [", col, "] not found in data.table during sep2 split. Skipping."), immediate. = TRUE, call. = FALSE)
      next
    }
    # Check if column is suitable for splitting (e.g., character)
    if (!is.character(dt_copy[[col]])) {
      warning(paste("Column [", col, "] is not character type. Skipping split."), immediate. = TRUE, call. = FALSE)
      next
    }
    
    # Perform the split using tstrsplit (replace with your method if different)
    split_result_list <- tryCatch({
      tstrsplit(dt_copy[[col]], sep_char, fixed = TRUE)
    }, error = function(e) {
      warning(paste("Error splitting column [", col, "]:", e$message), immediate. = TRUE, call. = FALSE)
      return(NULL) 
    })
    
    
    # Process the results if splitting was successful and produced columns
    if (!is.null(split_result_list) && length(split_result_list) > 0 && !all(sapply(split_result_list, is.null))) {
      max_splits_found <- length(split_result_list)
      new_col_names <- paste0(col, "_", seq_len(max_splits_found))
      
      # Add new columns and remove the original one from the copy
      dt_copy[, (new_col_names) := split_result_list]
      dt_copy[, (col) := NULL]
    }
  }
  
  return(dt_copy)
}

fread <- function(file, ..., sep2 = NULL, sep2cols = NULL) {
  
  # Step 1: Read using base fread
  dt <- data.table::fread(file = file, ...) # Pass file explicitly
  
  # Step 2: Apply splitting logic if requested
  if (!is.null(sep2) && !is.null(sep2cols) && length(sep2cols) > 0) {
    # Use the helper function for splitting
    dt <- apply_sep2_splitting(dt, sep2, sep2cols)
  } else if (!is.null(sep2) && (is.null(sep2cols) || length(sep2cols) == 0)) {
    warning("'sep2' was provided but 'sep2cols' was empty or NULL. No splitting performed.")
  }
  
  return(dt)
}
