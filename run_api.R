# run_api.R

# Check and install required packages
source("setup.R")

library(plumber)
library(logger)

# Set up logging
log_appender(appender_file("api.log"))
log_threshold(INFO)

# Source the API
api <- tryCatch({
  log_info("Loading API...")
  plumb("R/api.R")
}, error = function(e) {
  log_error(paste("Error loading API:", conditionMessage(e)))
  quit(status = 1)
})

# Run the API on port 8000
tryCatch({
  log_info("Starting API on port 8000...")
  api$run(port = 8000)
}, error = function(e) {
  log_error(paste("Error starting API:", conditionMessage(e)))
  quit(status = 1)
})