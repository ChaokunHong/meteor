# METEOR 

## Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms

<!-- badges: start -->
<!-- badges: end -->

METEOR is an R package designed for meta-analysis of antimicrobial resistance (AMR) data across different domains: human, animal, and environment. The package provides tools for data import, validation, analysis, visualization, and reporting.

## Features

- Import AMR data from various file formats (CSV, Excel, RData)
- Validate and standardize data for meta-analysis
- Perform meta-analysis with various methods and options
- Create static and interactive visualizations
- Analyze heterogeneity and publication bias
- Compare data across different domains (One Health approach)
- Interactive exploration via a Shiny application

## Installation

You can install the development version of meteor from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("username/meteor")
```

## Example

Basic usage example:

```r
library(meteor)

# Import data
amr_data <- import_amr_data("path/to/data.csv")

# Validate and standardize data
validated_data <- validate_data(amr_data, domain = "human")
std_data <- standardize_amr_data(validated_data, domain = "human")

# Perform meta-analysis
meta_results <- calculate_pooled_rate(
  std_data,
  by = c("pathogen", "antibiotic"),
  method = "random"
)

# Create forest plot
create_forest_plot(meta_results)
```

## Shiny Application

METEOR includes an interactive Shiny application for easier data analysis:

```r
# Launch the METEOR Shiny app
launch_meteor()
```

## Available Datasets

The package includes sample datasets:

```r
# Load human AMR data
data(human_data)
data(standardized_human_data)
```

## Documentation

For more detailed information, see the package vignettes:

```r
browseVignettes("meteor")
```

## Contributing

Contributions to METEOR are welcome! Please see CONTRIBUTING.md for guidelines.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- World Health Organization (WHO) for guidance on AMR monitoring
- R meta-analysis community and package developers 