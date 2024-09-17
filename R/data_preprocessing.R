# R/data_preprocessing.R

library(dplyr)
library(tidyr)
library(logger)

preprocess_data <- function(data) {
  log_info("Starting data preprocessing")
  
  tryCatch({
    # Remove rows with missing values
    data <- data %>% drop_na()
    
    # Convert effect size and standard error to numeric
    data <- data %>% 
      mutate(effect_size = as.numeric(effect_size),
             se = as.numeric(se))
    
    # Remove outliers (studies with effect sizes more than 3 SD from the mean)
    mean_es <- mean(data$effect_size)
    sd_es <- sd(data$effect_size)
    data <- data %>% 
      filter(effect_size >= mean_es - 3*sd_es & effect_size <= mean_es + 3*sd_es)
    
    # Standardize moderator variables if present
    if ("moderator" %in% names(data)) {
      data <- data %>% 
        mutate(moderator = scale(moderator))
    }
    
    log_info("Data preprocessing completed successfully")
    return(data)
  }, error = function(e) {
    log_error(paste("Error in data preprocessing:", e$message))
    stop(paste("Error in data preprocessing:", e$message))
  })
}
