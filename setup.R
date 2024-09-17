# Set a user-writable library path
user_lib <- Sys.getenv("R_LIBS_USER", unset = NA)
if (is.na(user_lib)) {
  user_lib <- "~/R/library"
}
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

# List of required packages
required_packages <- c(
  "plumber", "jsonlite", "future", "promises", "ggplot2", "logger", "DBI", "RPostgres"
)

# Function to check if package is available
check_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    cat(paste("Package", package, "is not available.\n"))
    return(FALSE)
  }
  return(TRUE)
}

# Check packages
all_available <- all(sapply(required_packages, check_package))

if (all_available) {
  cat("All required packages are available.\n")
} else {
  cat("Some required packages are missing. Please check the Nix configuration.\n")
  quit(status = 1)
}