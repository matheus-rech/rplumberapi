# R/sensitivity_analysis.R

library(metafor)
library(logger)
library(future)

perform_sensitivity_analysis <- function(data) {
  log_info("Starting sensitivity analysis")
  
  future({
    tryCatch({
      # Perform meta-analysis
      rma_result <- rma(yi = data$effect_size, sei = data$se, data = data)
      
      # Leave-one-out analysis
      leave_one_out <- leave1out(rma_result)
      
      # Influence analysis
      influence_analysis <- influence(rma_result)
      
      # Baujat plot
      baujat_plot <- baujat(rma_result)
      
      # Cumulative meta-analysis
      cumulative_analysis <- cumul(rma_result)
      
      list(
        leave_one_out = leave_one_out,
        influence_analysis = influence_analysis,
        baujat_plot = baujat_plot,
        cumulative_analysis = cumulative_analysis
      )
    }, error = function(e) {
      log_error(paste("Error in sensitivity analysis:", e$message))
      stop(paste("Error in sensitivity analysis:", e$message))
    })
  }) %...>% 
    (function(result) {
      log_info("Sensitivity analysis completed successfully")
      return(result)
    })
}
