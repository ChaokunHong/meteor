# Tests for core functions

library(testthat)
library(meteor)

# Test version function
test_that("meteor_version returns package version", {
  expect_type(meteor_version(), "character")
  expect_match(meteor_version(), "^\\d+\\.\\d+\\.\\d+$")
})

# Test options functions
test_that("options functions work correctly", {
  # Get default options
  default_options <- get_meteor_options()
  expect_type(default_options, "list")
  expect_true("default_meta_method" %in% names(default_options))
  
  # Set a new option
  set_meteor_options(confidence_level = 0.99)
  new_options <- get_meteor_options()
  expect_equal(new_options$confidence_level, 0.99)
  
  # Reset options
  set_meteor_options(reset = TRUE)
  reset_options <- get_meteor_options()
  expect_equal(reset_options$confidence_level, 0.95)
})

# Test initialization function with temporary directory
test_that("initialize_meteor_project creates correct structure", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  
  # Initialize a project
  project_name <- "test_project"
  project_path <- initialize_meteor_project(project_name, path = temp_dir)
  
  # Check that the main directory was created
  expect_true(dir.exists(file.path(temp_dir, project_name)))
  
  # Check for subdirectories
  subdirs <- c("data", "data/raw", "analysis", "results", "reports")
  for (subdir in subdirs) {
    expect_true(dir.exists(file.path(temp_dir, project_name, subdir)))
  }
  
  # Check for README file
  expect_true(file.exists(file.path(temp_dir, project_name, "README.md")))
  
  # Check for analysis script
  expect_true(file.exists(file.path(temp_dir, project_name, "analysis", paste0(project_name, "_analysis.R"))))
  
  # Clean up
  unlink(file.path(temp_dir, project_name), recursive = TRUE)
})

# Test check_dependencies function
test_that("check_dependencies identifies dependencies", {
  # Should return TRUE if required packages are available
  # Note: This test assumes the required packages are installed during package development
  expect_true(check_dependencies(quietly = TRUE))
})

# Test helper functions (placeholder for more detailed tests)
test_that("meteor_help returns expected output", {
  # Capture output to test
  output <- capture.output(meteor_help())
  expect_true(any(grepl("METEOR:", output)))
  expect_true(any(grepl("Data import:", output)))
  
  # Test topic-specific help
  output_import <- capture.output(meteor_help("import"))
  expect_true(any(grepl("Data Import Functions", output_import)))
}) 