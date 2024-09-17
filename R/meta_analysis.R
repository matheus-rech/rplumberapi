# R/meta_analysis.R

library(meta)
library(metafor)
library(dmetar)
library(metaSEM)
library(future)
library(logger)

perform_meta_analysis <- function(data, method = "random") {
  log_info("Starting meta-analysis")
  
  future({
    tryCatch({
      # Perform standard meta-analysis
      meta_result <- metagen(TE = data$effect_size, 
                             seTE = data$se, 
                             sm = method,
                             data = data,
                             studlab = data$study)
      
      # Enhanced funnel plot
      funnel_plot <- funnel(meta_result, 
                            yaxis = "sei",
                            xlim = c(-2, 2),
                            ylim = c(0.4, 0),
                            steps = 5,
                            back = "white",
                            hlines = "black")
      
      # Calculate fail-safe N
      fsn_result <- fsn(yi = data$effect_size, vi = data$se^2, type = "Rosenberg")
      
      # Enhanced forest plot
      forest_plot <- forest(meta_result,
                            sortvar = data$effect_size,
                            prediction = TRUE,
                            print.tau2 = TRUE,
                            col.predict = "red")
      
      # Perform publication bias assessment
      bias_test <- metabias(meta_result)
      
      # Perform trim and fill analysis
      trimfill_result <- trimfill(rma(yi = data$effect_size, sei = data$se))
      
      # Perform power analysis
      power_result <- dmetar::power.analysis(meta_result)
      
      # Perform outlier analysis
      outlier_result <- dmetar::find.outliers(meta_result)
      
      # Perform GOSH analysis
      gosh_result <- dmetar::gosh(meta_result, subsets = 1000)
      
      list(
        summary = summary(meta_result),
        heterogeneity = meta_result$I2,
        forest_plot = forest_plot,
        funnel_plot = funnel_plot,
        bias_test = bias_test,
        trimfill = trimfill_result,
        power_analysis = power_result,
        outlier_analysis = outlier_result,
        gosh_analysis = gosh_result,
        fsn = fsn_result
      )
    }, error = function(e) {
      log_error(paste("Error in meta-analysis:", e$message))
      stop(paste("Error in meta-analysis:", e$message))
    })
  }) %...>% 
    (function(result) {
      log_info("Meta-analysis completed successfully")
      return(result)
    })
}

perform_sem_meta_analysis <- function(data, model_formula) {
  log_info("Starting SEM meta-analysis")
  
  future({
    tryCatch({
      # Convert formula string to formula object
      model <- as.formula(model_formula)
      
      # Perform meta-analysis using metaSEM
      meta_sem_result <- meta(model, data = data)
      
      # Create forest plot
      forest_plot <- plot(meta_sem_result)
      
      # Perform publication bias analysis
      funnel_plot <- funnel(meta_sem_result)
      
      list(
        summary = summary(meta_sem_result),
        forest_plot = forest_plot,
        funnel_plot = funnel_plot,
        fit_indices = fit(meta_sem_result)
      )
    }, error = function(e) {
      log_error(paste("Error in SEM meta-analysis:", e$message))
      stop(paste("Error in SEM meta-analysis:", e$message))
    })
  }) %...>% 
    (function(result) {
      log_info("SEM meta-analysis completed successfully")
      return(result)
    })
}
