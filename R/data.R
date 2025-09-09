read_data <- function(filepath = NULL) {
  # If filepath is provided, use it; otherwise, prompt user to choose a file
  fpath <- filepath %||% file.choose()

  if (!file.exists(fpath) || !grepl("\\.csv$", fpath)) {
    stop("Please select a data file (.csv).")
  }

  # Read data from file
  data <- utils::read.csv(fpath, header = TRUE)

  data
}

