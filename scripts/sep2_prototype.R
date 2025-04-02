# scripts/sep2_prototype.R
fread <- function(..., sep2 = NULL, sep2cols = NULL) {
  # Use data.table's fread for the initial read
  dt <- data.table::fread(...)

  # --- START: Your sep2 implementation ---
  if (!is.null(sep2) && !is.null(sep2cols) && length(sep2cols) > 0) {
    if (!is.character(sep2) || length(sep2) != 1) {
      stop("'sep2' must be a single character string.")
    }
    if (!is.character(sep2cols) || !all(sep2cols %in% names(dt))) {
       stop("'sep2cols' must be a character vector of valid column names present in the data.")
    }

    message("Applying sep2 splitting...") # Optional progress message

    # Loop through specified columns and split them
    for (col in sep2cols) {
       # Use data.table::tstrsplit for efficiency
       new_cols <- paste0(col, "_", seq_len(max(lengths(strsplit(dt[[col]], sep2, fixed = TRUE))))) # Basic naming
       dt[, (new_cols) := data.table::tstrsplit(get(col), sep2, fixed = TRUE)]
       dt[, (col) := NULL] # Optionally remove the original column
    }
    # --- END: Your sep2 implementation ---
  }

  return(dt)
}