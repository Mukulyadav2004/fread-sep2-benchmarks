library(lobstr)  # For memory profiling

profile_memory <- function(file) {
  dt <- fread(file)
  
  # Manual splitting
  mem_manual <- mem_change({
    dt[, c("split1", "split2") := tstrsplit(nested_col1, ";")]
    dt[, c("splitA", "splitB") := tstrsplit(nested_col2, ";")]
  })
  
  # sep2 approach
  dt <- fread(file)
  mem_sep2 <- mem_change(
    dt <- fread(file, sep2 = ";", sep2cols = c("nested_col1", "nested_col2"))
  )
  
  data.table(
    Rows = nrow(dt),
    Method = c("Manual", "sep2"),
    Memory_MB = c(mem_manual, mem_sep2) / 1024^2
  )
}

# Run and save
results <- lapply(list.files("data", full.names = TRUE), profile_memory)
fwrite(rbindlist(results), "results/tables/memory_metrics.csv")