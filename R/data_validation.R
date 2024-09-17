# R/data_validation.R

library(validator)
library(logger)

validate_meta_analysis_data <- function(data) {
  log_info("Validating meta-analysis data")
  
  rules <- validator(
    effect_size = is.numeric(effect_size),
    se = is.numeric(se) & se > 0,
    study = is.character(study)
  )
  
  validation_result <- confront(data, rules)
  
  if (!all(validation_result)) {
    log_error("Invalid data for meta-analysis")
    stop("Invalid data for meta-analysis. Please check your input.")
  }
  
  log_info("Meta-analysis data validation successful")
  return(data)
}

validate_network_meta_analysis_data <- function(data) {
  log_info("Validating network meta-analysis data")
  
  rules <- validator(
    TE = is.numeric(TE),
    seTE = is.numeric(seTE) & seTE > 0,
    treat1 = is.character(treat1),
    treat2 = is.character(treat2)
  )
  
  validation_result <- confront(data, rules)
  
  if (!all(validation_result)) {
    log_error("Invalid data for network meta-analysis")
    stop("Invalid data for network meta-analysis. Please check your input.")
  }
  
  log_info("Network meta-analysis data validation successful")
  return(data)
}

validate_sensitivity_analysis_data <- function(data) {
  log_info("Validating sensitivity analysis data")
  
  rules <- validator(
    effect_size = is.numeric(effect_size),
    se = is.numeric(se) & se > 0
  )
  
  validation_result <- confront(data, rules)
  
  if (!all(validation_result)) {
    log_error("Invalid data for sensitivity analysis")
    stop("Invalid data for sensitivity analysis. Please check your input.")
  }
  
  log_info("Sensitivity analysis data validation successful")
  return(data)
}

validate_meta_regression_data <- function(data) {
  log_info("Validating meta-regression data")
  
  rules <- validator(
    effect_size = is.numeric(effect_size),
    se = is.numeric(se) & se > 0,
    moderator = is.numeric(moderator) | is.factor(moderator)
  )
  
  validation_result <- confront(data, rules)
  
  if (!all(validation_result)) {
    log_error("Invalid data for meta-regression")
    stop("Invalid data for meta-regression. Please check your input.")
  }
  
  log_info("Meta-regression data validation successful")
  return(data)
}

validate_visualization_data <- function(data) {
  log_info("Validating visualization data")
  
  rules <- validator(
    effect_size = is.numeric(effect_size),
    se = is.numeric(se) & se > 0,
    study = is.character(study)
  )
  
  validation_result <- confront(data, rules)
  
  if (!all(validation_result)) {
    log_error("Invalid data for visualization")
    stop("Invalid data for visualization. Please check your input.")
  }
  
  log_info("Visualization data validation successful")
  return(data)
}
