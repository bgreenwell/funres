
# funres

<!-- badges: start -->
[![R-CMD-check](https://github.com/bgreenwell/funres/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bgreenwell/funres/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The **funres** package provides an implementation of the functional residual framework described in [Liu et al. (2025)](). Functional residuals provide a novel diagnostic framework for generalized linear models (GLMs) and beyond, addressing limitations in traditional residuals for discrete outcome data. The framework uses functional residuals, which capture residual randomness that classical point statistics fail to represent, and provides new diagnostic tools like the functional-residual-vs-covariate and function-to-function plots. It broadens the diagnostic scope across various GLMs, including models for binary, ordinal, count data, and semiparametric models, offering a unified approach for both discrete and continuous data.

## Installation

Currently, you can only install the **funres** package from GitHub:
``` r
# install.packages("remotes")
remotes::install_github("bgreenwell/funres")
```
