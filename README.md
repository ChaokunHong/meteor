# meteor: Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms

<!-- badges: start -->
[![R-CMD-check](https://github.com/ChaokunHong/meteor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ChaokunHong/meteor/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ChaokunHong/meteor/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ChaokunHong/meteor?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/meteor)](https://CRAN.R-project.org/package=meteor)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

The `meteor` package is an R toolkit for meta-analysis of antimicrobial resistance (AMR) data across human, animal, and environmental domains. It provides a comprehensive set of functions for data import, validation, analysis, visualization, and reporting of AMR patterns.

## Features

- **Data Management**: Import, validate, and standardize AMR data from various sources
- **Meta-Analysis**: Conduct pooled analyses, heterogeneity assessments, subgroup analyses, and more
- **Visualization**: Create forest plots, geographic maps, heatmaps, and interactive visualizations
- **Shiny Applications**: Interactive dashboards for data exploration and analysis
- **Prediction**: Forecast AMR trends and simulate intervention impacts
- **Reporting**: Generate customized reports for different audiences

## Installation

You can install the development version of meteor from GitHub:

```r
# Install from GitHub
devtools::install_github("ChaokunHong/meteor")
```

## Usage

```r
library(meteor)

# Launch the Shiny application
launch_meteor()

# Or use specific functions
data <- import_amr_data("your_data.csv")
validated_data <- validate_data(data)
results <- calculate_pooled_rate(validated_data)
plot <- create_forest_plot(results)
```

## Data

The package includes curated AMR data from:

- Human clinical studies
- Animal studies (forthcoming)
- Environmental studies (forthcoming)

Users can also upload their own data for comparison with existing meta-analyses.

## Documentation

See the vignettes and help pages for detailed documentation:

```r
browseVignettes("meteor")
help(package = "meteor")
```

## Examples

### Meta-analysis of resistance rates

```r
# Load built-in human AMR data
data(human_amr)

# Filter to specific pathogens and antibiotics
filtered_data <- filter_amr_data(
  human_amr,
  pathogen = c("Ecoil", "KP"),
  antibiotic = c("CIP", "TET")
)

# Run meta-analysis
results <- calculate_pooled_rate(
  data = filtered_data,
  by = c("pathogen", "antibiotic"),
  method = "random"
)

# Visualize results
create_forest_plot(results)
```

### Interactive exploration

```r
# Launch Shiny dashboard with custom data
data(human_amr)
launch_meteor(data = human_amr)
```

## License

MIT

## Citation

If you use this package in your research, please cite:

```
@software{meteor2023,
  author = {Hong, Chaokun},
  title = {meteor: Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms},
  url = {https://github.com/ChaokunHong/meteor},
  version = {0.1.0},
  year = {2023},
}
```

## Contributing

Contributions are welcome! Please feel free to submit a pull request or open an issue on our [GitHub repository](https://github.com/ChaokunHong/meteor).

## Contact

For questions or feedback, please contact Chaokun Hong at chaokun.hong@gmail.com.

## Code of Conduct

Please note that the meteor project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms. 