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

# Function to check and install package if not available
check_and_install_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    cat(paste("Attempting to install package", package, "\n"))
    install.packages(package, repos = "https://cloud.r-project.org")
    if (!requireNamespace(package, quietly = TRUE)) {
      cat(paste("Failed to install package", package, "\n"))
      return(FALSE)
    }
  }
  return(TRUE)
}

# Check and install packages
all_available <- all(sapply(required_packages, check_and_install_package))

if (all_available) {
  cat("All required packages are available.\n")
} else {
  cat("Some required packages are missing and could not be installed. Please check your R environment.\n")
  quit(status = 1)
}