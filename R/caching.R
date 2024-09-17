# R/caching.R

library(memoise)
library(digest)

# Create a cache with a 1-hour expiration
mem_cache <- function(expire = 3600) {
  cache <- new.env(parent = emptyenv())
  
  list(
    get = function(key) {
      value <- cache[[key]]
      if (!is.null(value) && (value$timestamp + expire > Sys.time())) {
        return(value$data)
      }
      NULL
    },
    set = function(key, value) {
      cache[[key]] <- list(data = value, timestamp = Sys.time())
      value
    }
  )
}

# Memoize functions with caching
memoize_with_cache <- function(f, cache) {
  function(...) {
    key <- digest(list(f, ...))
    result <- cache$get(key)
    if (is.null(result)) {
      result <- f(...)
      cache$set(key, result)
    }
    result
  }
}

# Apply caching to analysis functions
perform_meta_analysis_cached <- memoize_with_cache(perform_meta_analysis, mem_cache())
perform_network_meta_analysis_cached <- memoize_with_cache(perform_network_meta_analysis, mem_cache())
perform_sensitivity_analysis_cached <- memoize_with_cache(perform_sensitivity_analysis, mem_cache())
perform_meta_regression_cached <- memoize_with_cache(perform_meta_regression, mem_cache())
