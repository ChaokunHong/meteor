# Script to build and install the meteor package

# Clean and rebuild documentation
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Cleaning and rebuilding documentation...")
  devtools::document()
} else {
  message("devtools package not found. Installing...")
  install.packages("devtools")
  devtools::document()
}

# Build the package
message("Building the package...")
pkg_file <- devtools::build()

# Install the package
message("Installing the package...")
install.packages(pkg_file, repos = NULL, type = "source")

message("Package installation complete!")
message("Try running: library(meteor); launch_meteor()") 