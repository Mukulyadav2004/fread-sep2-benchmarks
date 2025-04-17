# benchmarks/runtime.R
library(data.table)
library(microbenchmark)
library(ggplot2)
library(scales) # For scales::label_number

# Basic settings for this script
benchmark_times <- 10
nested_separator <- ";"
cols_to_split <- c("nested_col1", "nested_col2")
results_dir_tab <- "results/tables"
results_dir_fig <- "results/figures"
raw_results_file <- file.path(results_dir_tab, "runtime_microbenchmark_raw.rds")
agg_results_file <- file.path(results_dir_tab, "runtime_metrics_aggregated.csv")
plot_file <- file.path(results_dir_fig, "runtime_plot.png")

source("scripts/sep2_prototype.R")

# Make sure the results directories exist
dir.create(results_dir_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir_fig, recursive = TRUE, showWarnings = FALSE)

# Define the main benchmarking function - NOW BENCHMARKS SPLITTING LOGIC
benchmark_sep2 <- function(file, sep_char, cols_split, times_run) {
  message(paste("Benchmarking Splitting Logic for:", basename(file)))
  
  # Read data ONCE, outside the benchmark
  dt_orig <- tryCatch({
    data.table::fread(file)
  }, error = function(e) {
    stop("Error reading file:", file, "\n", conditionMessage(e))
  })
  
  # Check if columns to split actually exist in the read data
  missing_cols <- setdiff(cols_split, names(dt_orig))
  if(length(missing_cols) > 0) {
    warning("Columns specified in 'cols_to_split' not found in file '", basename(file), "': ", paste(missing_cols, collapse=", "), ". Skipping these columns for this file.", call. = FALSE)
    cols_split <- intersect(cols_split, names(dt_orig))
    if(length(cols_split) == 0) {
      warning("No columns left to split for file '", basename(file), "'. Skipping benchmark for this file.", call. = FALSE)
      return(NULL) # Return NULL if no columns to split
    }
  }
  # Compare splitting logic on in-memory copies
  mb_result <- microbenchmark(
    # Manual splitting approach (applied to a copy of pre-read data)
    manual = {
      dt_manual <- copy(dt_orig)
      for(col in cols_split) {
        if (!is.character(dt_manual[[col]])) next 
        split_result_list <- tstrsplit(dt_manual[[col]], sep_char, fixed = TRUE)
        
        if (!is.null(split_result_list) && length(split_result_list) > 0 && !all(sapply(split_result_list, is.null))) {
          max_splits_found <- length(split_result_list)
          new_col_names <- paste0(col, "_", seq_len(max_splits_found))
          dt_manual[, (new_col_names) := split_result_list]
          dt_manual[, (col) := NULL]
        } else {
          warning(paste("Column [", col, "] in file [", basename(file), "] was not split. Keeping it as is."), immediate. = TRUE)
        }
      }
    },
    # Use the custom sep2 splitting logic HELPER function (applied to pre-read data)
    sep2 = {
      # Pass the ALREADY READ data to the splitting function
      dt_sep2 <- apply_sep2_splitting(dt_orig, sep_char, cols_split)
    },
    times = times_run,
    unit = "ms"
    # Removed check = "equal" for simplicity during debugging
  )
  
  # Store the filename with the result object for later retrieval
  attr(mb_result, "filename") <- file
  return(mb_result)
}

plot_results <- function(results_list, agg_file, plot_out_file) {
  message("Processing and plotting runtime results...")
  
  results_list <- Filter(Negate(is.null), results_list) 

  if (length(results_list) == 0 || all(sapply(results_list, function(x) !inherits(x, "microbenchmark")))) {
    warning("No valid benchmark data to process (list is empty or contains non-microbenchmark objects).")
    return(NULL)
  }
  
  processed_results <- lapply(results_list, function(mb_result) {
    if (!inherits(mb_result, "microbenchmark")) return(NULL)
    file_path <- attr(mb_result, "filename")
    if (is.null(file_path)) {
      warning("Couldn't identify the file name from benchmark result attribute.", immediate. = TRUE, call. = FALSE)
      return(NULL)
    }
    base_filename <- basename(file_path)
    
    rows <- NA 
    pattern <- "test_([0-9.e+]+)\\.csv$" 
    
    if (grepl(pattern, base_filename)) { 
      row_count_str <- sub(pattern, "\\1", base_filename)
      rows <- as.numeric(row_count_str) 
      if (is.na(rows)) {
        warning(paste("Could not convert extracted row count string to numeric:", row_count_str, "from file:", base_filename), immediate. = TRUE, call. = FALSE)
        rows <- NA
      }
    } else {
      warning(paste("Filename pattern did not match:", base_filename), immediate. = TRUE, call. = FALSE)
      rows <- NA
    }
    
    if (is.na(rows)) {
      warning(paste("Skipping result due to failed row count extraction for file:", base_filename), immediate. = TRUE, call. = FALSE)
      return(NULL) # Return NULL to be filtered out later
    }
    
    stats_summary <- summary(mb_result)
    
    data.table(
      File = base_filename,
      Rows = rows,
      Method = stats_summary$expr,
      MedianTime_ms = stats_summary$median # microbenchmark unit was set to 'ms'
    )
  })
  
  # Filter out any NULLs that resulted from processing errors
  processed_results <- Filter(Negate(is.null), processed_results)
  
  if (length(processed_results) == 0) {
    warning("No results remaining after processing individual benchmark objects.", call. = FALSE)
    return(NULL)
  }
  
  # Combine into a single data table
  df <- rbindlist(processed_results)
  
  # Filter out any remaining rows with potential issues (shouldn't be needed if NULL filtering above works)
  df <- df[!is.na(Rows) & !is.na(Method) & !is.na(MedianTime_ms)]
  if (nrow(df) == 0) {
    warning("No benchmark data to plot after aggregation and filtering.", call. = FALSE)
    return(NULL)
  }
  
  # Sort output for readability
  setorder(df, Rows, Method)
  
  print("Summary of median execution times (ms):")
  print(df)
  
  # Save aggregated metrics
  message(paste("Writing aggregated results to:", agg_file))
  fwrite(df, agg_file)
  
  # Create and save the plot
  message("Creating runtime comparison plot...")
  p <- ggplot(df, aes(x = Rows, y = MedianTime_ms, color = Method, group = Method)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    scale_x_log10(
      breaks = unique(df$Rows),
      labels = scales::label_number(big.mark = ",", accuracy=1)
    ) +
    scale_y_log10(labels = scales::label_number(big.mark = ",")) +
    labs(
      title = "Runtime Comparison: Custom 'sep2' Splitting Logic vs Manual",
      subtitle = paste("Median of", benchmark_times, "runs per data size (Splitting only, file read excluded)"),
      y = "Median Execution Time (ms, log scale)",
      x = "Dataset Size (Number of Rows, log scale)",
      color = "Method"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face="bold"))
  
  message(paste("Saving runtime plot to:", plot_out_file))
  ggsave(plot_out_file, plot = p, width = 8, height = 6, dpi = 150)
  
  return(p)
}

# Main script entry point
data_files <- list.files("data", pattern = "^test_.*\\.csv$", full.names = TRUE)
if (length(data_files) == 0) {
  stop("No CSV data files found. Please run data/generate_data.R first.")
}
message(paste("Found", length(data_files), "data files to benchmark."))

runtime_results_list <- lapply(
  data_files,
  function(f) { 
    tryCatch({
      benchmark_sep2(
        file = f,
        sep_char = nested_separator,
        cols_split = cols_to_split,
        times_run = benchmark_times
      )
    }, error = function(e) {
      warning("Error benchmarking file '", basename(f), "': ", conditionMessage(e), call. = FALSE)
      return(NULL) 
    })
  }
)

# Filter out NULLs from the list before saving (e.g., if benchmark_sep2 returned NULL)
runtime_results_list <- Filter(Negate(is.null), runtime_results_list)

if(length(runtime_results_list) > 0) {
  message(paste("Saving raw microbenchmark results object to:", raw_results_file))
  saveRDS(runtime_results_list, raw_results_file)
} else {
  message("No benchmark results were generated to save.")
}


runtime_plot <- plot_results(runtime_results_list, agg_results_file, plot_file)

# Check if plotting failed
if(is.null(runtime_plot)) {
  message("Plotting failed or produced no output.")
} else {
  message("Runtime plot generated successfully.")
}

message("Runtime benchmarking script completed.")