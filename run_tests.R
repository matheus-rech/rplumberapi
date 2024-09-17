#!/usr/bin/env Rscript

# run_tests.R

library(testthat)
library(logger)

# Set up logging
log_appender(appender_file("test_results.log"))
log_threshold(INFO)

# Define the test directory
test_dir <- "tests"

# Run all tests in the test directory
log_info("Starting test suite...")
test_results <- test_dir(test_dir)

# Print test results
print(test_results)

# Calculate test statistics
total_tests <- sum(test_results$n)
passed_tests <- sum(test_results$passed)
failed_tests <- total_tests - passed_tests

# Log test results
log_info(paste("Total tests:", total_tests))
log_info(paste("Passed tests:", passed_tests))
log_info(paste("Failed tests:", failed_tests))

# Exit with appropriate status code
if (failed_tests > 0) {
  log_error("Some tests failed. Check the log for details.")
  quit(status = 1)
} else {
  log_info("All tests passed successfully.")
  quit(status = 0)
}