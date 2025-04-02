fread <- function(..., sep2 = NULL, sep2cols = NULL) {
  dt <- data.table::fread(...)
  
  if (!is.null(sep2)) {
    # [Include your full validation/splitting logic from earlier]
  }
  
  return(dt)
}