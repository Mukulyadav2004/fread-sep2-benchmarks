# benchmarks/runtime.R
library(data.table)
library(microbenchmark)
library(ggplot2)

# Basic settings for this script
benchmark_times <- 10
nested_separator <- ";"
cols_to_split <- c("nested_col1", "nested_col2")
results_dir_tab <- "results/tables"
results_dir_fig <- "results/figures"
raw_results_file <- file.path(results_dir_tab, "runtime_microbenchmark_raw.rds")
agg_results_file <- file.path(results_dir_tab, "runtime_metrics_aggregated.csv")
plot_file <- file.path(results_dir_fig, "runtime_plot.png")

# Load the custom sep2-based fread function
source("scripts/sep2_prototype.R")

# Make sure the results directories exist
dir.create(results_dir_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir_fig, recursive = TRUE, showWarnings = FALSE)

# Define the main benchmarking function
benchmark_sep2 <- function(file, sep_char, cols_split, times_run) {
  message(paste("Benchmarking Runtime:", basename(file)))
  
  # Attempt to read data from the file; stop if there's an error
  dt_orig <- tryCatch({
    data.table::fread(file)
  }, error = function(e) {
    stop(...)
  })
  
  # Compare manual splitting with the custom sep2 approach
  mb_result <- microbenchmark(
    # Manual splitting approach
    manual = {
      dt_manual <- copy(dt_orig)
      
      # Begin manual splitting column by column
      for(col in cols_split) {
        split_result_list <- tstrsplit(dt_manual[[col]], sep_char, fixed = TRUE)
        
        if (length(split_result_list) > 0 && !all(sapply(split_result_list, is.null))) {
          max_splits_found <- length(split_result_list)
          new_col_names <- paste0(col, "_", seq_len(max_splits_found))
          dt_manual[, (new_col_names) := split_result_list]
          dt_manual[, (col) := NULL]
        } else {
          warning(paste("Column [", col, "] in file [", basename(file), "] was not split. Keeping it as is."), immediate. = TRUE)
        }
      }
    },
    # Use the custom fread that implements sep2 logic
    sep2 = {
      dt_sep2 <- fread(file, sep2 = sep_char, sep2cols = cols_split)
    },
    times = times_run,
    unit = "ms"
  )
  
  mb_result$file <- file
  return(mb_result)
}

# Process benchmark results and generate plots
plot_results <- function(results_list, agg_file, plot_out_file) {
  message("Processing and plotting runtime results...")
  
  # Check for valid microbenchmark objects
  if (length(results_list) == 0 || all(sapply(results_list, function(x) !inherits(x, "microbenchmark")))) {
    warning("No valid benchmark data to process.")
    return(NULL)
  }
  
  # Combine all benchmarks into one table
  df <- rbindlist(lapply(results_list, function(mb_result) {
    if (!inherits(mb_result, "microbenchmark")) return(NULL)
    
    file_path <- tryCatch(unique(mb_result$file), error = function(e) NA)
    if (is.na(file_path)) {
      warning("Couldn't identify the file name from benchmark result.")
      return(NULL)
    }
    
    # Attempt to extract the number of rows from the file name
    row_match <- regmatches(basename(file_path), regexpr("test_([0-9e\\+\\.]+)\\.csv", basename(file_path)))
    if (length(row_match) > 0 && length(row_match[[1]]) >= 2) {
      rows <- as.numeric(row_match[[1]][2])
    } else {
      warning(paste("Unable to find row count in file name:", basename(file_path)))
      rows <- NA
    }
    
    stats_summary <- summary(mb_result)
    
    data.table(
      File = basename(file_path),
      Rows = rows,
      Method = stats_summary$expr,
      MedianTime_ms = stats_summary$median
    )
  }), fill = TRUE)
  
  # Filter out unprocessable rows
  df <- df[!is.na(Rows) & !is.na(Method) & !is.na(MedianTime_ms)]
  if (nrow(df) == 0) {
    warning("No benchmark data to plot after processing.")
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
      title = "Runtime Comparison: Custom 'sep2' vs Manual Splitting",
      subtitle = paste("Median of", benchmark_times, "runs per data size"),
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
  benchmark_sep2,
  sep_char = nested_separator,
  cols_split = cols_to_split,
  times_run = benchmark_times
)

message(paste("Saving raw microbenchmark results to:", raw_results_file))
saveRDS(runtime_results_list, raw_results_file)

runtime_plot <- plot_results(runtime_results_list, agg_results_file, plot_file)
message("Runtime benchmarking script completed successfully.")