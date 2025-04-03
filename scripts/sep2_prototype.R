# scripts/sep2_prototype.R
# This function wraps data.table's fread and lets you split columns further using 'sep2'
fread <- function(..., sep2 = NULL, sep2cols = NULL) {
  # First, read the data using the standard data.table::fread
  dt <- data.table::fread(...)
  
  # Check if we need to apply the secondary separator logic
  if (!is.null(sep2) && !is.null(sep2cols) && length(sep2cols) > 0) {
    if (!is.character(sep2) || length(sep2) != 1) {
      stop("'sep2' must be a single string character.")
    }
    if (!is.character(sep2cols) || !all(sep2cols %in% names(dt))) {
      stop("'sep2cols' must be a character vector of valid column names in the data.")
    }
    
    message("Splitting specified columns using 'sep2'...")
    
    # Go through each listed column and split it on 'sep2'
    for (col in sep2cols) {
      # Figure out how many splits we'll need
      new_cols <- paste0(
        col, "_",
        seq_len(max(lengths(strsplit(dt[[col]], sep2, fixed = TRUE))))
      )
      
      dt[, (new_cols) := data.table::tstrsplit(get(col), sep2, fixed = TRUE)]
      dt[, (col) := NULL]  # Remove the original column once it's been split
    }
  }
  
  return(dt)
}