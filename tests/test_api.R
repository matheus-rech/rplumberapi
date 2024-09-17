# tests/test_api.R

library(testthat)
library(httr)
library(jsonlite)

base_url <- "http://localhost:8000"

# Helper function to make API calls
make_api_call <- function(endpoint, data, method = "POST") {
  response <- POST(
    url = paste0(base_url, endpoint),
    body = data,
    encode = "json"
  )
  list(
    status = status_code(response),
    content = if (status_code(response) == 200) content(response, "parsed") else content(response, "text")
  )
}

# Test data
meta_analysis_data <- list(
  effect_size = c(0.1, 0.2, 0.3),
  se = c(0.05, 0.05, 0.05),
  study = c("Study 1", "Study 2", "Study 3")
)

network_meta_data <- list(
  TE = c(0.1, 0.2, 0.3),
  seTE = c(0.05, 0.05, 0.05),
  treat1 = c("A", "B", "C"),
  treat2 = c("B", "C", "A")
)

meta_regression_data <- list(
  effect_size = c(0.1, 0.2, 0.3),
  se = c(0.05, 0.05, 0.05),
  moderator = c(1, 2, 3)
)

# Test suite
test_that("API endpoints return expected results", {
  
  test_that("Meta-analysis endpoint works", {
    result <- make_api_call("/perform_meta_analysis", meta_analysis_data)
    expect_equal(result$status, 200)
    expect_true("summary" %in% names(result$content))
    expect_true("heterogeneity" %in% names(result$content))
  })
  
  test_that("Network meta-analysis endpoint works", {
    result <- make_api_call("/perform_network_meta_analysis", network_meta_data)
    expect_equal(result$status, 200)
    expect_true("summary" %in% names(result$content))
    expect_true("forest_plot" %in% names(result$content))
  })
  
  test_that("Sensitivity analysis endpoint works", {
    result <- make_api_call("/perform_sensitivity_analysis", meta_analysis_data)
    expect_equal(result$status, 200)
    expect_true("leave_one_out" %in% names(result$content))
    expect_true("influence_analysis" %in% names(result$content))
  })
  
  test_that("Meta-regression endpoint works", {
    result <- make_api_call("/perform_meta_regression", meta_regression_data)
    expect_equal(result$status, 200)
    expect_true("summary" %in% names(result$content))
    expect_true("bubble_plot" %in% names(result$content))
  })
  
  test_that("Visualization endpoint works", {
    result <- make_api_call("/generate_visualization", 
                            list(data = meta_analysis_data, plot_type = "forest"))
    expect_equal(result$status, 200)
    expect_true(grepl("image/png", result$content$headers$`Content-Type`))
  })
  
  test_that("Data preprocessing endpoint works", {
    result <- make_api_call("/preprocess_data", meta_analysis_data)
    expect_equal(result$status, 200)
    expect_true(is.list(result$content))
    expect_true(all(c("effect_size", "se", "study") %in% names(result$content)))
  })
  
  test_that("API handles errors correctly", {
    invalid_data <- list(invalid = "data")
    result <- make_api_call("/perform_meta_analysis", invalid_data)
    expect_equal(result$status, 500)
    expect_true(grepl("error", result$content, ignore.case = TRUE))
  })
})

# Run the tests
test_results <- test_file("tests/test_api.R")
print(test_results)
