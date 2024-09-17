# R/api.R

required_packages <- c("plumber", "jsonlite", "memoise", "future", "promises", "validator", "meta", "metafor", "netmeta", "ggplot2", "logger", "ratelimitr", "dmetar", "metaSEM")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

source("utils/data_validation.R")
source("utils/error_handling.R")
source("analysis/meta_analysis.R")
source("analysis/network_meta_analysis.R")
source("analysis/sensitivity_analysis.R")
source("analysis/meta_regression.R")

#* @apiTitle MetaGPT API

# ... (keep other existing endpoints)

#* Perform Meta-Regression
#* @param data JSON string of input data
#* @post /perform_meta_regression
function(req, res) {
  tryCatch({
    data <- jsonlite::fromJSON(req$postBody)
    validated_data <- validate_meta_regression_data(data)
    
    # Initialize logging for meta-regression
    initialize_meta_regression_logging("meta_regression_api.log")
    
    result <- perform_meta_regression(validated_data)
    res$setHeader("Content-Type", "application/json")
    res$body <- jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE)
    res
  }, error = function(e) {
    log_error(paste("Error in meta-regression API:", e$message))
    handle_error(e, res)
  })
}

# ... (keep other existing endpoints and configurations)

# Set up logging
log_appender(appender_file("api.log"))
log_threshold(INFO)

# Define rate limiter
rate_limiter <- limit_rate(rate = 10, period = 60)

# Apply rate limiter to all endpoints
#* @filter rate_limit
function(req, res) {
  rate_limiter(function() {
    plumber::forward()
  })()
}

# Log API start
log_info("API started on port 8000")

# Run the API
# To start the API, run this script using `Rscript run_api.R`

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}