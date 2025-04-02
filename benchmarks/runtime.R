library(data.table)
library(microbenchmark)
source("../scripts/sep2_prototype.R")  # Your implementation

# Benchmark function
benchmark_sep2 <- function(file) {
  dt <- fread(file)
  
  microbenchmark(
    manual = {
      dt[, c("split1", "split2") := tstrsplit(nested_col1, ";")]
      dt[, c("splitA", "splitB") := tstrsplit(nested_col2, ";")]
    },
    sep2 = fread(file, sep2 = ";", sep2cols = c("nested_col1", "nested_col2")),
    times = 10
  )
}

# Run for all datasets
results <- lapply(list.files("data", full.names = TRUE), benchmark_sep2)
saveRDS(results, "results/tables/runtime_metrics.rds")