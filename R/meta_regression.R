# R/meta_regression.R

library(metafor)
library(logger)
library(future)

perform_meta_regression <- function(data) {
  log_info("Starting meta-regression")
  
  future({
    tryCatch({
      # Perform meta-regression
      meta_reg_result <- rma(yi = data$effect_size, 
                             sei = data$se, 
                             mods = ~ data$moderator, 
                             data = data)
      
      # Generate bubble plot
      bubble_plot <- bubble(meta_reg_result, 
                            xlab = "Moderator", 
                            ylab = "Effect Size")
      
      # Perform residual analysis
      residual_plot <- plot(meta_reg_result, 
                            xlab = "Fitted Values", 
                            ylab = "Residuals")
      
      # Perform moderator analysis
      moderator_analysis <- anova(meta_reg_result)
      
      list(
        summary = summary(meta_reg_result),
        bubble_plot = bubble_plot,
        residual_plot = residual_plot,
        moderator_analysis = moderator_analysis
      )
    }, error = function(e) {
      log_error(paste("Error in meta-regression:", e$message))
      stop(paste("Error in meta-regression:", e$message))
    })
  }) %...>% 
    (function(result) {
      log_info("Meta-regression completed successfully")
      return(result)
    })
}
