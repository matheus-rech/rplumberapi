library(metafor)
library(futile.logger)

perform_sensitivity_analysis <- function(data) {
  tryCatch({
    # Input validation
    if (!is.data.frame(data) || !"effect_size" %in% names(data) || !"se" %in% names(data)) {
      stop("Invalid input parameters for sensitivity analysis")
    }
    
    flog.info("Performing sensitivity analysis")
    rma_result <- rma(yi = data$effect_size, sei = data$se, data = data)
    sens_result <- leave1out(rma_result)
    
    list(
      summary = summary(sens_result),
      plot = plot(sens_result)
    )
  }, error = function(e) {
    flog.error("Error in perform_sensitivity_analysis: %s", e$message)
    stop(paste("Error in perform_sensitivity_analysis:", e$message))
  })
}