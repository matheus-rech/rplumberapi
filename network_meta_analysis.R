library(netmeta)
library(futile.logger)
library(dmetar)

perform_network_meta_analysis <- function(data) {
  tryCatch({
    # Input validation
    if (!is.data.frame(data) || !all(c("TE", "seTE", "treat1", "treat2") %in% names(data))) {
      stop("Invalid input parameters for network meta-analysis")
    }
    
    flog.info("Performing network meta-analysis")
    netmeta_result <- netmeta(TE, seTE, treat1, treat2, data = data, sm = "SMD")
    
    flog.info("Calculating SUCRA")
    sucra_result <- sucra(netmeta_result)
    
    list(
      summary = summary(netmeta_result),
      forest_plot = forest(netmeta_result),
      net_graph = netgraph(netmeta_result),
      sucra = sucra_result
    )
  }, error = function(e) {
    flog.error("Error in perform_network_meta_analysis: %s", e$message)
    stop(paste("Error in perform_network_meta_analysis:", e$message))
  })
}