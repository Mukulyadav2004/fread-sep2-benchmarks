# fread-sep2 Benchmarks

**GSoC Proposal: Performance Highlights**

## Project Goal

This repository showcases how adding a secondary separator parameter (`sep2`) into `data.table::fread` could potentially boost both speed and memory efficiency. 

We compare two methods:

1. **Manual Post-Processing:** The current approach of reading with `data.table::fread` and then using `tstrsplit` to further split specific columns.
2. **Prototype `sep2`:** A simple R-based function in `scripts/sep2_prototype.R` that demonstrates handling `sep2` and `sep2cols` during file read.

Although the prototype is in R, these tests highlight how a future native implementation in `fread` (likely in C) can offer notable performance gains.

## Benchmarks Performed

- **Runtime:** Run multiple iterations with the `microbenchmark` package on datasets of various sizes (10k, 100k, 1M rows). The results are saved in `results/tables/runtime_metrics_aggregated.csv` and a chart is created in `results/figures/runtime_plot.png`.
- **Memory Usage:** Uses R's `gc()` to measure how much additional memory each method requires, saving results to `results/tables/memory_metrics_gc.csv`.

## Prerequisites

- R (Version 4.0 or later suggested)
- RStudio (Recommended IDE)
- Git
- Required R packages: `data.table`, `microbenchmark`, `ggplot2`

## How to Reproduce Results

1. **Clone the Repository:**
    Open your terminal or command prompt.
    ```bash
    git clone https://github.com/Mukulyadav2004/fread-sep2-benchmarks.git
    cd fread-sep2-benchmarks
    ```

2.  **Open the Project in RStudio:**
    Navigate to the cloned directory and open the `fread-sep2-benchmarks.Rproj` file.

3.  **Install Required Packages:**
    If you haven't already, run this command in the RStudio **Console**:
    ```R
    install.packages(c("data.table", "microbenchmark", "ggplot2"))
    ```

4.  **Run Benchmark Scripts:**
    Open the **Terminal** tab within RStudio (`Tools` -> `Terminal` -> `New Terminal`). Make sure the terminal prompt shows you are in the `fread-sep2-benchmarks` project root directory. Execute the following commands one by one:

    *   **Generate synthetic test data:**
        ```bash
        Rscript data/generate_data.R
        ```
    *   **Run runtime benchmarks & generate plot:**
        ```bash
        Rscript benchmarks/runtime.R
        ```
    *   **Run memory benchmarks:**
        ```bash
        Rscript benchmarks/memory.R
        ```

## Understanding the Results

*   **Runtime:** Examine the plot `results/figures/runtime_plot.png` and the table `results/tables/runtime_metrics_aggregated.csv`.
    *   The plot visually compares the median time (in milliseconds, log scale) for the `Manual` vs. `sep2` methods as the number of rows increases (log scale).
    *   **Expected Outcome:** The line for the `sep2` method should be consistently lower than the `Manual` line, indicating faster execution times.

*   **Memory Usage:** Examine the table `results/tables/memory_metrics_gc.csv`.
    *   This table shows the estimated increase in memory usage (in MB) for each method.
    *   `Manual Splitting (...)` measures the memory delta of *just the splitting step* after the data is already read.
    *   `sep2 (...)` measures the memory delta of the *combined read + split* operation performed by the prototype.
    *   **Expected Outcome:** The `sep2` method should show a lower memory increase (`Memory_Diff_MB`) compared to the `Manual` method for each file size.
    *   **Note on `gc()` Measurement:** While the `gc()` method provides a good relative comparison, the `sep2` value includes the read operation's memory cost, whereas `Manual` only includes the split cost. The fact that `sep2 (Read + Split)` still typically uses less memory than `Manual (Split Only)` strongly suggests significant potential memory savings from an integrated approach.

## Important Note for Mentor

The `scripts/sep2_prototype.R` file contains a **simplified R prototype** designed solely for demonstrating the *concept* and running these benchmarks. It is **not** an optimized C implementation. The positive results from this prototype serve as evidence that a proper, efficient implementation integrated within `fread`'s C code (as proposed for GSoC) is likely to yield substantial performance improvements.
