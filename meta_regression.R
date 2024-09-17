library(metafor)
library(logger)

#' Initialize logging for meta-regression module
#'
#' @param log_file Path to the log file
#' @return The path to the log file
#' @export
initialize_meta_regression_logging <- function(log_file = "meta_regression.log") {
  # Ensure the directory exists
  dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
  
  # Set up the logger
  log_appender(appender_file(log_file))
  log_threshold(INFO)
  
  # Write an initial log message to ensure the file is created
  log_info("Meta-regression logging initialized")
  
  return(log_file)
}

#' Perform meta-regression analysis
#'
#' @param data A data frame containing the following columns:
#'   - effect_size: The effect size for each study
#'   - se: The standard error of the effect size for each study
#'   - moderator: The moderator variable for the meta-regression
#' @param log_file Path to the log file (optional)
#' @return The meta-regression results (rma object)
#' @export
perform_meta_regression <- function(data, log_file = NULL) {
  # Initialize logging if not already done
  if (is.null(log_file)) {
    log_file <- initialize_meta_regression_logging()
  } else {
    # Ensure logging is set up for the provided log file
    log_appender(appender_file(log_file))
    log_threshold(INFO)
  }

  # Input validation
  if (!is.data.frame(data)) {
    log_error("Input must be a data frame")
    stop("Input must be a data frame")
  }
  
  required_columns <- c("effect_size", "se", "moderator")
  if (!all(required_columns %in% colnames(data))) {
    missing_columns <- setdiff(required_columns, colnames(data))
    log_error(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  if (any(is.na(data[, required_columns]))) {
    log_error("NA values found in required columns")
    stop("NA values found in required columns")
  }
  
  # Perform meta-regression
  tryCatch({
    log_info("Starting meta-regression analysis")
    rma_result <- rma(yi = data$effect_size, sei = data$se, mods = ~ data$moderator, data = data)
    log_info("Meta-regression analysis completed successfully")
    return(rma_result)  # Return the rma object directly
  }, error = function(e) {
    log_error(paste("Error in meta-regression analysis:", e$message))
    stop(paste("Error in meta-regression analysis:", e$message))
  }, warning = function(w) {
    log_warn(paste("Warning in meta-regression analysis:", w$message))
  })
}