library(meta)
library(metafor)
library(futile.logger)
library(dmetar)
library(metaSEM)

perform_meta_analysis <- function(data, effect_size, method, reference = NULL, control_event_rate = NULL, subgroups = NULL) {
  tryCatch({
    # Input validation
    if (!is.data.frame(data) || !effect_size %in% names(data) || !is.character(method)) {
      stop("Invalid input parameters")
    }
    
    # Perform standard meta-analysis
    flog.info("Performing meta-analysis with method: %s", method)
    meta_result <- metagen(TE = data[[effect_size]], 
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
    fsn_result <- fsn(yi = data[[effect_size]], vi = data$se^2, type = "Rosenberg")
    
    # Enhanced forest plot
    forest_plot <- forest(meta_result,
                          sortvar = data[[effect_size]],
                          prediction = TRUE,
                          print.tau2 = TRUE,
                          col.predict = "red")
    
    # Extract model formula
    model_formula <- formula(meta_result)
    
    # Perform publication bias assessment
    flog.info("Assessing publication bias")
    bias_test <- metabias(meta_result)
    
    # Perform trim and fill analysis
    flog.info("Performing trim and fill analysis")
    trimfill_result <- trimfill(rma(yi = data[[effect_size]], sei = data$se))
    
    # Perform power analysis
    flog.info("Performing power analysis")
    power_result <- dmetar::power.analysis(meta_result)
    
    # Perform outlier analysis
    flog.info("Performing outlier analysis")
    outlier_result <- dmetar::find.outliers(meta_result)
    
    # Perform GOSH analysis
    flog.info("Performing GOSH analysis")
    gosh_result <- dmetar::gosh(meta_result, subsets = 1000)
    
    # Calculate NNT if control event rate is provided
    nnt_result <- NULL
    if (!is.null(control_event_rate)) {
      flog.info("Calculating Number Needed to Treat")
      nnt_result <- NNT(meta_result, CER = control_event_rate)
    }
    
    # Perform Risk of Bias summary if data is available
    rob_result <- NULL
    if ("rob" %in% names(data)) {
      flog.info("Generating Risk of Bias summary")
      rob_result <- rob.summary(data$rob)
    }
   
    # Perform subgroup analysis if subgroups are provided
    subgroup_result <- NULL
    if (!is.null(subgroups)) {
      flog.info("Performing subgroup analysis")
      subgroup_result <- subgroup.analysis.mixed.effects(meta_result, subgroups)
    }
    
    # Return summary and additional useful information
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
      nnt = nnt_result,
      rob_summary = rob_result,
      subgroup_analysis = subgroup_result,
      fsn = fsn_result,
      model_formula = model_formula
    )
  }, error = function(e) {
    flog.error("Error in perform_meta_analysis: %s", e$message)
    stop(paste("Error in perform_meta_analysis:", e$message))
  })
}

perform_sem_meta_analysis <- function(data, model_formula) {
  tryCatch({
    # Input validation
    if (!is.data.frame(data) || !is.character(model_formula)) {
      stop("Invalid input parameters for SEM meta-analysis")
    }
    
    flog.info("Performing SEM meta-analysis")
    
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
    flog.error("Error in perform_sem_meta_analysis: %s", e$message)
    stop(paste("Error in perform_sem_meta_analysis:", e$message))
  })
}