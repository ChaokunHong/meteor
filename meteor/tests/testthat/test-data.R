# Tests for data functions

library(testthat)
library(meteor)

# Create a mock data frame for testing
create_mock_data <- function() {
  data.frame(
    `study ID` = c("study1", "study2", "study3"),
    firstauthor_last = c("Smith", "Jones", "Lee"),
    year_published = c(2019, 2020, 2021),
    location = c("USA", "UK", "China"),
    population = c("Adults", "Children", "Adults"),
    Environment = c("Hospital", "Community", "Hospital"),
    n_pop = c(100, 150, 200),
    r_CIP_Ecoil = c(0.25, 0.3, 0.35),
    n_CIP_Ecoil = c(25, 45, 70),
    d_CIP_Ecoil = c(100, 150, 200),
    r_AMP_Ecoil = c(0.4, 0.45, 0.5),
    n_AMP_Ecoil = c(40, 67, 100),
    d_AMP_Ecoil = c(100, 150, 200),
    stringsAsFactors = FALSE
  )
}

# Test validate_data function
test_that("validate_data validates correctly", {
  mock_data <- create_mock_data()
  
  # Test with valid data
  validated <- validate_data(mock_data, domain = "human", strict = FALSE)
  expect_true(attr(validated, "validated"))
  expect_equal(attr(validated, "domain"), "human")
  
  # Test with missing required columns
  incomplete_data <- mock_data[, -which(names(mock_data) == "firstauthor_last")]
  expect_warning(validate_data(incomplete_data, domain = "human", strict = FALSE))
  
  # Test with invalid resistance rates
  invalid_data <- mock_data
  invalid_data$r_CIP_Ecoil[1] <- 1.5  # Invalid rate > 1
  expect_warning(validate_data(invalid_data, domain = "human", strict = FALSE))
  
  # Test strict mode with invalid data
  expect_error(validate_data(incomplete_data, domain = "human", strict = TRUE))
})

# Test check_data_quality function
test_that("check_data_quality evaluates quality", {
  mock_data <- create_mock_data()
  
  # Test quality check
  quality_result <- check_data_quality(mock_data, domain = "human")
  expect_true(is.list(attr(quality_result, "quality")))
  expect_true("completeness" %in% names(attr(quality_result, "quality")))
  
  # Test quality score
  quality_score <- check_data_quality(mock_data, domain = "human", return_score = TRUE)
  expect_type(quality_score, "double")
  expect_true(quality_score >= 0 && quality_score <= 1)
  
  # Test with invalid data
  invalid_data <- mock_data
  invalid_data$r_CIP_Ecoil[1] <- 1.5  # Invalid rate > 1
  quality_invalid <- check_data_quality(invalid_data, domain = "human")
  expect_true(length(attr(quality_invalid, "quality")$issues) > 0)
})

# Test standardize_amr_data function
test_that("standardize_amr_data converts data correctly", {
  mock_data <- create_mock_data()
  
  # Standardize data
  std_data <- standardize_amr_data(mock_data, domain = "human")
  
  # Check structure
  expect_true("study_id" %in% names(std_data))
  expect_true("pathogen" %in% names(std_data))
  expect_true("antibiotic" %in% names(std_data))
  expect_true("resistance_rate" %in% names(std_data))
  
  # Check content conversion
  expect_equal(nrow(std_data), 6)  # 3 studies x 2 antibiotics
  expect_true(any(std_data$antibiotic == "Ciprofloxacin"))
  expect_true(any(std_data$antibiotic == "Ampicillin"))
  expect_true(all(std_data$pathogen == "Escherichia coli"))
  
  # Check attributes
  expect_true(attr(std_data, "standardized"))
  expect_equal(attr(std_data, "domain"), "human")
})

# Test impute_missing_data function
test_that("impute_missing_data handles missing values", {
  # Create data with missing values
  data_with_na <- create_mock_data()
  data_with_na$r_CIP_Ecoil[2] <- NA
  data_with_na$n_CIP_Ecoil[2] <- NA
  
  # Test mean imputation
  imputed_mean <- impute_missing_data(data_with_na, method = "mean", cols = "r_CIP_Ecoil")
  expect_false(any(is.na(imputed_mean$r_CIP_Ecoil)))
  expected_value <- mean(c(data_with_na$r_CIP_Ecoil[1], data_with_na$r_CIP_Ecoil[3]), na.rm = TRUE)
  expect_equal(imputed_mean$r_CIP_Ecoil[2], expected_value)
  
  # Test removal of rows with NA
  imputed_remove <- impute_missing_data(data_with_na, method = "remove", cols = "r_CIP_Ecoil")
  expect_equal(nrow(imputed_remove), 2)
  
  # Test with no missing values
  complete_data <- create_mock_data()
  message <- capture_messages(
    imputed_complete <- impute_missing_data(complete_data, method = "mean", cols = "r_CIP_Ecoil")
  )
  expect_match(message, "No missing values")
  expect_identical(imputed_complete, complete_data)
}) 