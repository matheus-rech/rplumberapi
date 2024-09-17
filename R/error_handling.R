# R/error_handling.R

library(logger)

handle_error <- function(e, res) {
  error_message <- paste("Error:", e$message)
  log_error(error_message)
  
  res$status <- 500
  res$body <- list(error = error_message)
  res
}

log_and_return_error <- function(error_message) {
  log_error(error_message)
  list(error = error_message)
}
