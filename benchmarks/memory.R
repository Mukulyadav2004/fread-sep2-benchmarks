library(data.table)

# setup vars
nested_separator <- ";"
cols_to_split <- c("nested_col1", "nested_col2")
results_dir_tab <- "results/tables"
results_file <- file.path(results_dir_tab, "memory_metrics_gc.csv")

# grab the custom fread function w/ sep2 support
source("scripts/sep2_prototype.R")

# make sure output dir exists
dir.create(results_dir_tab, recursive = TRUE, showWarnings = FALSE)

# main memory profiling function - uses gc() to track mem usage
profile_memory_gc <- function(file, sep_char, cols_split) {
  message(paste("Profiling Memory (using gc()):", basename(file)))
  
  # first approach: manual splitting 
  dt_manual_orig <- data.table::fread(file)
  rows_manual <- nrow(dt_manual_orig)
  gc(reset = TRUE, full = TRUE)
  mem_before_manual <- gc()[, 1]
  
  # do the manual splitting
  dt_manual <- copy(dt_manual_orig) # need a copy to work with
  for(col in cols_split) {
    # split once per column
    split_result_list <- tstrsplit(dt_manual[[col]], sep_char, fixed = TRUE)
    
    # check if we got any actual splits
    if(length(split_result_list) > 0 && !all(sapply(split_result_list, is.null))) {
      # figure out how many pieces we got
      max_splits_found <- length(split_result_list)
      # make col names like col_1, col_2, etc
      new_col_names <- paste0(col, "_", seq_len(max_splits_found))
      # add the new columns
      dt_manual[, (new_col_names) := split_result_list]
      # get rid of original col
      dt_manual[, (col) := NULL]
    } else {
      # handle edge case where separator isn't found
      warning(paste("Column [", col, "] in file [", basename(file), "] resulted in no splits during memory profile. Check data/separator. Keeping original column."), immediate. = TRUE)
      # could remove orig col anyway but probably not what user wants
    }
  }
  
  mem_after_manual <- gc()[, 1]
  mem_diff_manual <- sum(mem_after_manual) - sum(mem_before_manual)
  rm(dt_manual_orig, dt_manual); gc(full = TRUE)
  
  # second approach: sep2 method
  gc(reset = TRUE, full = TRUE)
  mem_before_sep2 <- gc()[, 1]
  dt_sep2 <- fread(file, sep2 = sep_char, sep2cols = cols_split) # our custom fread
  mem_after_sep2 <- gc()[, 1]
  mem_diff_sep2 <- sum(mem_after_sep2) - sum(mem_before_sep2)
  rows_sep2 <- if (exists("dt_sep2") && is.data.frame(dt_sep2)) nrow(dt_sep2) else NA
  rm(dt_sep2); gc(full = TRUE)
  
  # put results together
  result_dt <- data.table(
    File = basename(file), Rows = c(rows_manual, rows_sep2),
    Method = c("Manual Splitting (Post-Read, GC Memory Diff of Split)", "sep2 (GC Memory Diff of Read + Split)"),
    Memory_Diff_Bytes = c(mem_diff_manual, mem_diff_sep2),
    Memory_Diff_MB = c(mem_diff_manual, mem_diff_sep2) / (1024^2)
  )
  return(result_dt)
}

# main script execution
data_files <- list.files("data", pattern = "^test_.*\\.csv$", full.names = TRUE)
if (length(data_files) == 0) { stop("FATAL: No CSV data files found in data/. Run data/generate_data.R first.") }
message(paste("Found", length(data_files), "data files to profile memory using gc()."))
memory_results_list <- lapply(data_files, profile_memory_gc, sep_char = nested_separator, cols_split = cols_to_split)
memory_results_agg <- rbindlist(memory_results_list)
setorder(memory_results_agg, Rows, Method)
print("Aggregated Memory Profiling Results (using gc()):")
print(memory_results_agg)
message(paste("Saving memory metrics (gc based) to:", results_file))
fwrite(memory_results_agg, results_file)
message("Memory benchmarking script (gc based) finished successfully.")