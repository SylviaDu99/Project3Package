<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/SylviaDu99/Project3Package.svg?branch=master)](https://travis-ci.org/SylviaDu99/Project3Package)
  [![Codecov test coverage](https://codecov.io/gh/SylviaDu99/Project3Package/branch/master/graph/badge.svg)](https://codecov.io/gh/SylviaDu99/Project3Package?branch=master)
<!-- badges: end -->

# Project3Package

This package is a final project of STAT 302 in UW containing 4 functions we 
developed during the quarter that allow us to analyze data with multiple methods.

## Installation

To install Project3Package, use the code below:

```{r, eval = FALSE}
devtools::install_github("SylviaDu99/Project3Package")
```
## Use

The vignette demonstrates example usage of all main functions. To see the vignette, use the following code (note that this requires a TeX installation to view properly):

```{r, eval = FALSE}
devtools::install_github("SylviaDu99/Project3Package", build_vignette = TRUE, build_opts = c())
library(Project3Package)
# Use this to view the vignette in the Project3Package HTML help
help(package = "Project3Package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "Project3Package")
```
