library(data.table)
set.seed(123)

# Generate 1M rows with nested columns
create_test_data <- function(n_rows, sep = ";") {
  data.table(
    id = 1:n_rows,
    nested_col1 = paste(sample(100, n_rows, replace = TRUE), 
                        sample(100, n_rows, replace = TRUE), 
                        sep = sep),
    nested_col2 = paste(sample(letters, n_rows, replace = TRUE), 
                        sample(LETTERS, n_rows, replace = TRUE), 
                        sep = sep)
  )
}

# Save datasets of varying sizes
sizes <- c(1e4, 1e5, 1e6)  # 10k, 100k, 1M rows
for (n in sizes) {
  fwrite(create_test_data(n), file = paste0("data/test_", n, ".csv"))
}