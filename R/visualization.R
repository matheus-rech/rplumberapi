# R/visualization.R

library(ggplot2)
library(metafor)
library(dplyr)
library(logger)

generate_visualization <- function(data, plot_type) {
  log_info(paste("Generating", plot_type, "visualization"))
  
  tryCatch({
    switch(plot_type,
           "forest" = generate_forest_plot(data),
           "funnel" = generate_funnel_plot(data),
           "radial" = generate_radial_plot(data),
           stop(paste("Unsupported plot type:", plot_type))
    )
  }, error = function(e) {
    log_error(paste("Error in generate_visualization:", e$message))
    stop(paste("Error in generate_visualization:", e$message))
  })
}

generate_forest_plot <- function(data) {
  rma_result <- rma(yi = data$effect_size, sei = data$se, data = data)
  forest_plot <- forest(rma_result, slab = data$study, xlab = "Effect Size", refline = 0)
  return(forest_plot)
}

generate_funnel_plot <- function(data) {
  rma_result <- rma(yi = data$effect_size, sei = data$se, data = data)
  funnel_plot <- funnel(rma_result, main = "Funnel Plot", xlab = "Effect Size")
  return(funnel_plot)
}

generate_radial_plot <- function(data) {
  rma_result <- rma(yi = data$effect_size, sei = data$se, data = data)
  radial_plot <- radial(rma_result)
  return(radial_plot)
}
