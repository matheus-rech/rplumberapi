# R/network_meta_analysis.R

library(netmeta)
library(logger)
library(future)

perform_network_meta_analysis <- function(data) {
  log_info("Starting network meta-analysis")
  
  future({
    tryCatch({
      # Perform network meta-analysis
      netmeta_result <- netmeta(TE = data$TE, 
                                seTE = data$seTE, 
                                treat1 = data$treat1, 
                                treat2 = data$treat2, 
                                data = data, 
                                sm = "SMD")
      
      # Calculate SUCRA
      sucra_result <- sucra(netmeta_result)
      
      # Generate network graph
      net_graph <- netgraph(netmeta_result, 
                            plastic = FALSE, 
                            thickness = "number of studies",
                            col = "steelblue")
      
      # Generate comparison-adjusted funnel plot
      funnel_plot <- funnel(netmeta_result)
      
      # Generate league table
      league_table <- netleague(netmeta_result, 
                                digits = 2, 
                                bracket = "(", 
                                separator = " to ")
      
      # Perform inconsistency analysis
      inconsistency <- netsplit(netmeta_result)
      
      list(
        summary = summary(netmeta_result),
        forest_plot = forest(netmeta_result),
        net_graph = net_graph,
        sucra = sucra_result,
        funnel_plot = funnel_plot,
        league_table = league_table,
        inconsistency = inconsistency
      )
    }, error = function(e) {
      log_error(paste("Error in network meta-analysis:", e$message))
      stop(paste("Error in network meta-analysis:", e$message))
    })
  }) %...>% 
    (function(result) {
      log_info("Network meta-analysis completed successfully")
      return(result)
    })
}
