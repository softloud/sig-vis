# sigvis <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/softloud/sig-vis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/softloud/sig-vis/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A package for visualizing structured intelligence governance (SIG) systems using R6 classes and network visualization.

## Installation

You can install the development version of sigvis from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("softloud/sig-vis")
```

## Overview

The `sigvis` package provides three main R6 classes for working with structured intelligence governance data:

- **`SigDat`**: Loads data from Google Sheets or uses template data
- **`SigGraph`**: Constructs tidygraph objects from SIG data  
- **`SigVis`**: Creates network visualizations with ggraph

## Quick Start

```r
library(sigvis)

# Explore template data
head(template_edges)
head(template_nodes)

# Create a mock workflow (full workflow requires Google Sheets setup)
mock_sig_dat <- list(
  get_edges = function() template_edges,
  get_nodes = function() template_nodes
)

# Build graph
sig_graph <- SigGraph$new(sig_dat = mock_sig_dat)

# Create visualization  
sig_vis <- SigVis$new(sig_graph = sig_graph)
plot <- sig_vis$plot()
print(plot)
```

## Features

- **Data Loading**: Support for Google Sheets integration with template fallback
- **Graph Construction**: tidygraph-based network creation with node aggregation
- **Visualization**: ggraph-powered network plots with customizable themes
- **Validation**: Input validation and error handling throughout
- **Testing**: Comprehensive test suite with testthat
- **Documentation**: Full roxygen2 documentation and pkgdown website

## Data Structure

### Edges
- `from`, `to`: Source and target nodes
- `to_minimum_requirements`: Description of requirements  
- `arrowkeeper`: Entity responsible for the connection
- `status`: Current state (operational, buggy, not developed)

### Nodes
- `node`: Name of the component
- `node_context`: Categorical grouping (humans, projects, tools, etc.)

## Documentation

For detailed documentation and examples, see the [package website](https://softloud.github.io/sig-vis/) or use:

```r
# View main class documentation
?SigDat
?SigGraph  
?SigVis

# View template data documentation
?template_edges
?template_nodes

# View vignettes
vignette("getting-started", package = "sigvis")
```

## Contributing

Please see the [contributing guidelines](CONTRIBUTING.md) for information on how to contribute to this project.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.