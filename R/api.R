# R/api.R

library(plumber)
library(jsonlite)
library(memoise)
library(future)
library(promises)
library(validator)
library(meta)
library(metafor)
library(netmeta)
library(ggplot2)
library(logger)
library(ratelimitr)
library(dmetar)
library(metaSEM)
library(DBI)
library(RPostgres)

source("R/meta_analysis.R")
source("R/network_meta_analysis.R")
source("R/sensitivity_analysis.R")
source("R/meta_regression.R")
source("R/data_validation.R")
source("R/error_handling.R")
source("R/caching.R")
source("R/authentication.R")
source("R/visualization.R")
source("R/data_preprocessing.R")

#* @apiTitle MetaGPT API
#* @apiDescription A comprehensive R-based API for various types of meta-analysis

# Set up logging
log_appender(appender_file("api.log"))
log_threshold(INFO)

# Set up caching
cache <- mem_cache()

# Set up rate limiting
rate_limiter <- limit_rate(rate = 10, period = 60)

# Database connection
db_conn <- dbConnect(RPostgres::Postgres(),
                     dbname = Sys.getenv("PGDATABASE"),
                     host = Sys.getenv("PGHOST"),
                     port = Sys.getenv("PGPORT"),
                     user = Sys.getenv("PGUSER"),
                     password = Sys.getenv("PGPASSWORD"))

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* @filter authenticate
function(req, res) {
  if (!is_authenticated(req)) {
    res$status <- 401
    return(list(error = "Unauthorized"))
  } else {
    plumber::forward()
  }
}

#* @filter rate_limit
function(req, res) {
  rate_limiter(function() {
    plumber::forward()
  })()
}

#* Perform Meta-Analysis
#* @param data JSON string of input data
#* @param method Meta-analysis method (e.g., "fixed", "random")
#* @post /perform_meta_analysis
function(req, res) {
  tryCatch({
    data <- jsonlite::fromJSON(req$postBody)
    validated_data <- validate_meta_analysis_data(data$data)
    method <- data$method
    
    result <- perform_meta_analysis(validated_data, method)
    
    res$setHeader("Content-Type", "application/json")
    return(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE))
  }, error = function(e) {
    log_error(paste("Error in meta-analysis API:", e$message))
    handle_error(e, res)
  })
}

#* Perform Network Meta-Analysis
#* @param data JSON string of input data
#* @post /perform_network_meta_analysis
function(req, res) {
  tryCatch({
    data <- jsonlite::fromJSON(req$postBody)
    validated_data <- validate_network_meta_analysis_data(data)
    
    result <- perform_network_meta_analysis(validated_data)
    
    res$setHeader("Content-Type", "application/json")
    return(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE))
  }, error = function(e) {
    log_error(paste("Error in network meta-analysis API:", e$message))
    handle_error(e, res)
  })
}

#* Perform Sensitivity Analysis
#* @param data JSON string of input data
#* @post /perform_sensitivity_analysis
function(req, res) {
  tryCatch({
    data <- jsonlite::fromJSON(req$postBody)
    validated_data <- validate_sensitivity_analysis_data(data)
    
    result <- perform_sensitivity_analysis(validated_data)
    
    res$setHeader("Content-Type", "application/json")
    return(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE))
  }, error = function(e) {
    log_error(paste("Error in sensitivity analysis API:", e$message))
    handle_error(e, res)
  })
}

#* Perform Meta-Regression
#* @param data JSON string of input data
#* @post /perform_meta_regression
function(req, res) {
  tryCatch({
    data <- jsonlite::fromJSON(req$postBody)
    validated_data <- validate_meta_regression_data(data)
    
    result <- perform_meta_regression(validated_data)
    
    res$setHeader("Content-Type", "application/json")
    return(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE))
  }, error = function(e) {
    log_error(paste("Error in meta-regression API:", e$message))
    handle_error(e, res)
  })
}

#* Generate Visualization
#* @param data JSON string of input data
#* @param plot_type Type of plot to generate
#* @post /generate_visualization
function(req, res) {
  tryCatch({
    data <- jsonlite::fromJSON(req$postBody)
    validated_data <- validate_visualization_data(data$data)
    plot_type <- data$plot_type
    
    result <- generate_visualization(validated_data, plot_type)
    
    res$setHeader("Content-Type", "image/png")
    res$body <- result
    res
  }, error = function(e) {
    log_error(paste("Error in visualization API:", e$message))
    handle_error(e, res)
  })
}

#* Preprocess Data
#* @param data JSON string of input data
#* @post /preprocess_data
function(req, res) {
  tryCatch({
    data <- jsonlite::fromJSON(req$postBody)
    
    result <- preprocess_data(data)
    
    res$setHeader("Content-Type", "application/json")
    return(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE))
  }, error = function(e) {
    log_error(paste("Error in data preprocessing API:", e$message))
    handle_error(e, res)
  })
}

# Shutdown hook to close database connection
#* @plumber
function(pr) {
  pr$registerHook("exit", function() {
    dbDisconnect(db_conn)
    log_info("Database connection closed")
  })
}

#* @get /hello
function() {
  list(message = "Hello, world!")
}

#* @post /echo
function(msg = "") {
  list(message = paste0("The message is: '", msg, "'"))
}
