library(testthat)
library(httr)
library(jsonlite)

base_url <- "http://localhost:8000"

# Helper function to make API calls
make_api_call <- function(endpoint, data, analysis_type = NULL) {
  body <- if (is.null(analysis_type)) list(data = data) else list(data = data, analysisType = analysis_type)
  response <- POST(
    url = paste0(base_url, endpoint),
    body = body,
    encode = "json"
  )
  list(
    status = status_code(response),
    content = if (status_code(response) == 200) content(response, "parsed") else content(response, "text")
  )
}

# Test data
standard_data <- list(effect_size = c(0.1, 0.2, 0.3), se = c(0.05, 0.05, 0.05))
network_data <- list(TE = c(0.1, 0.2, 0.3), seTE = c(0.05, 0.05, 0.05), treat1 = c("A", "B", "C"), treat2 = c("B", "C", "A"))
sem_data <- list(x = c(1, 2, 3), y = c(2, 4, 6))

# Test suite
test_that("API endpoints return expected results", {
  
  # Test standard meta-analysis
  test_that("Standard meta-analysis works", {
    result <- make_api_call("/perform_meta_analysis", standard_data, "standard")
    expect_equal(result$status, 200)
    expect_true("summary" %in% names(result$content))
    expect_true("heterogeneity" %in% names(result$content))
  })
  
  # Test network meta-analysis
  test_that("Network meta-analysis works", {
    result <- make_api_call("/perform_network_meta_analysis", network_data)
    expect_equal(result$status, 200)
    expect_true("summary" %in% names(result$content))
    expect_true("forest_plot" %in% names(result$content))
  })
  
  # Test sensitivity analysis
  test_that("Sensitivity analysis works", {
    result <- make_api_call("/perform_sensitivity_analysis", standard_data)
    expect_equal(result$status, 200)
    expect_true("summary" %in% names(result$content))
    expect_true("plot" %in% names(result$content))
  })
  
  # Test SEM meta-analysis
  test_that("SEM meta-analysis works", {
    result <- make_api_call("/perform_sem_meta_analysis", sem_data, "sem")
    expect_equal(result$status, 200)
    expect_true("summary" %in% names(result$content))
    expect_true("fit_indices" %in% names(result$content))
  })
  
  # Test error handling
  test_that("API handles errors correctly", {
    result <- make_api_call("/perform_meta_analysis", list(invalid_data = "error"), "standard")
    expect_equal(result$status, 500)
    expect_true(grepl("error", result$content, ignore.case = TRUE))
  })
})

# Run the tests
test_results <- test_file("test_api.R")
print(test_results)