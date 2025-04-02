# benchmarks/memory.R
library(data.table) # Needed for fread, fwrite, tstrsplit etc.
library(lobstr)     # For memory profiling: mem_change

# Ensure this path is correct relative to the benchmark script location
source("../scripts/sep2_prototype.R") # Your fread implementation

profile_memory <- function(file) {
  message(paste("Profiling memory for:", basename(file)))

  # --- Manual splitting ---
  # Read data *outside* mem_change to measure only the splitting operation memory
  dt_manual_orig <- data.table::fread(file) # Use base fread
  mem_manual <- lobstr::mem_change({
      dt_manual <- copy(dt_manual_orig) # Work on a copy
      # Perform the splitting operations
      dt_manual[, c("nested_col1_split1", "nested_col1_split2") := tstrsplit(nested_col1, ";", fixed=TRUE)]
      dt_manual[, c("nested_col2_splitA", "nested_col2_splitB") := tstrsplit(nested_col2, ";", fixed=TRUE)]
      # Optionally remove original columns if your sep2 does
      # dt_manual[, c("nested_col1", "nested_col2") := NULL]
  })
  # Clean up memory explicitly (optional, but good practice in loops)
  rm(dt_manual_orig, dt_manual)
  gc() # Garbage collect

  # --- sep2 approach ---
  # Measure memory change including the read AND split by your prototype
  # Note: This measures read+split, while manual measured only split.
  # For a fairer comparison, you might read outside mem_change here too,
  # but the prompt implies measuring the whole sep2 call.
  mem_sep2 <- lobstr::mem_change({
      # Use the sourced prototype 'fread' here
      dt_sep2 <- fread(file, sep2 = ";", sep2cols = c("nested_col1", "nested_col2"))
  })
   # Get row count from the result of the sep2 read
  rows <- if (exists("dt_sep2") && is.data.frame(dt_sep2)) nrow(dt_sep2) else NA
  # Clean up memory
  rm(dt_sep2)
  gc()

  # Return results as a data.table
  data.table(
      File = basename(file),
      Rows = rows, # Use measured rows
      Method = c("Manual Splitting (Post-Read)", "sep2 (Read + Split)"),
      Memory_Change_Bytes = c(mem_manual, mem_sep2),
      Memory_Change_MB = c(mem_manual, mem_sep2) / (1024^2)
  )
}

# --- Run for all datasets ---
data_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
 if (length(data_files) == 0) {
   stop("No CSV data files found in data/ directory. Run data/generate_data.R first.")
}

# Use lapply to run profiling on each file
# Use suppressMessages if prototype `fread` is verbose
results_list <- lapply(data_files, profile_memory)

# Combine results into one table
memory_results <- rbindlist(results_list)

print("Memory Profiling Results:")
print(memory_results)

# Save the results table
fwrite(memory_results, "results/tables/memory_metrics.csv")
message("Memory metrics saved to results/tables/memory_metrics.csv")

message("Memory benchmarking script finished.")