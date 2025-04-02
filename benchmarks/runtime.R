# benchmarks/runtime.R
library(data.table)
library(microbenchmark)
library(ggplot2) # Added for plotting

# Ensure this path is correct relative to the benchmark script location
source("../scripts/sep2_prototype.R")  # Your implementation

# --- Benchmark Function ---
benchmark_sep2 <- function(file) {
  # Pre-read once to avoid including initial read time in manual split
  dt_orig <- fread(file) # Use data.table::fread here to be sure

  message(paste("Benchmarking:", basename(file)))

  mb_result <- microbenchmark(
    manual = {
      dt_manual <- copy(dt_orig) # Work on a copy
      dt_manual[, c("nested_col1_split1", "nested_col1_split2") := tstrsplit(nested_col1, ";", fixed=TRUE)]
      dt_manual[, c("nested_col2_splitA", "nested_col2_splitB") := tstrsplit(nested_col2, ";", fixed=TRUE)]
      # Optionally remove original columns if your sep2 does
      # dt_manual[, c("nested_col1", "nested_col2") := NULL]
    },
    # Use the sourced prototype 'fread' here
    sep2 = {
       dt_sep2 <- fread(file, sep2 = ";", sep2cols = c("nested_col1", "nested_col2"))
    },
    times = 10 # Number of benchmark repetitions
  )

  # Add file info to the result for later aggregation
  mb_result$file <- file
  return(mb_result)
}

# --- Run for all datasets ---
# Ensure the script is run from the project root OR adjust path
# Assuming running via Rscript from root, list.files("data/...") works
# If sourcing interactively from benchmarks/, use list.files("../data/...")
data_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
if (length(data_files) == 0) {
   stop("No CSV data files found in data/ directory. Run data/generate_data.R first.")
}

# Use lapply to run benchmark on each file
# Use suppressMessages to keep output clean if prototype `fread` is verbose
results_list <- lapply(data_files, benchmark_sep2)

# Save raw microbenchmark results (optional but good practice)
saveRDS(results_list, "results/tables/runtime_microbenchmark_raw.rds")

# --- Process and Plot Results ---
# Function to extract relevant summary info and plot
plot_results <- function(results_list) {
    # Check if results_list is empty or contains errors
    if (length(results_list) == 0 || any(sapply(results_list, inherits, "try-error"))) {
        warning("No valid benchmark results to plot.")
        return(NULL) # Return NULL if no data
    }

    # Aggregate results into a single data.table
    df <- rbindlist(lapply(results_list, function(mb_result) {
        # Check if mb_result is a valid microbenchmark object
        if (!inherits(mb_result, "microbenchmark")) return(NULL)

        # Extract file name and number of rows
        file_path <- unique(mb_result$file) # Get the file path stored earlier
        # Improved regex to handle potential variations in filename format
        row_match <- regmatches(basename(file_path), regexpr("test_([0-9e\\+\\.]+)\\.csv", basename(file_path)))
         if (length(row_match) == 0 || length(row_match[[1]]) < 2) {
            warning(paste("Could not extract row count from filename:", file_path))
            rows <- NA
        } else {
            rows <- as.numeric(row_match[[1]][2])
        }

        # Get summary statistics (median time in nanoseconds)
        summary_stats <- summary(mb_result)

        data.table(
            File = basename(file_path),
            Rows = rows,
            Method = summary_stats$expr,
            MedianTime_ns = summary_stats$median, # Median time in nanoseconds
            MedianTime_ms = summary_stats$median / 1e6 # Convert to milliseconds
        )
    }), fill = TRUE) # Use fill=TRUE in case some runs failed

    # Remove rows with NA Rows if extraction failed
    df <- df[!is.na(Rows)]

    if (nrow(df) == 0) {
        warning("No processable benchmark data after aggregation.")
        return(NULL)
    }

    print("Aggregated Runtime Results:")
    print(df)

    # Save the aggregated table
    fwrite(df, "results/tables/runtime_metrics_aggregated.csv")

    # Create the plot
    p <- ggplot(df, aes(x = Rows, y = MedianTime_ms, color = Method, group = Method)) +
            geom_line(linewidth = 1.2) +
            geom_point(size = 2) + # Add points for clarity
            scale_x_log10(breaks = unique(df$Rows), labels = scales::comma) + # Log scale for rows, show actual numbers
            scale_y_log10(labels = scales::comma) + # Log scale for time often useful
            labs(
                title = "Runtime Comparison: Custom sep2 vs Manual Splitting",
                y = "Median Execution Time (ms, log scale)",
                x = "Dataset Size (Number of Rows, log scale)",
                color = "Method"
            ) +
            theme_minimal() +
            theme(legend.position = "bottom")

    # Save the plot
    ggsave("results/figures/runtime_plot.png", plot = p, width = 8, height = 6)
    message("Runtime plot saved to results/figures/runtime_plot.png")

    return(p) # Return the plot object
}

# --- Execute the plotting function ---
runtime_plot <- plot_results(results_list)
# Optionally display the plot in RStudio Plots pane if run interactively
# if (!is.null(runtime_plot)) { print(runtime_plot) }

message("Runtime benchmarking script finished.")