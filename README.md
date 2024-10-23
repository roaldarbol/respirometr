
<!-- README.md is generated from README.Rmd. Please edit that file -->

# respirometr

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/773406370.svg)](https://zenodo.org/doi/10.5281/zenodo.13235277)
[![R-CMD-check](https://github.com/roaldarbol/respirometr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/roaldarbol/respirometr/actions/workflows/R-CMD-check.yaml)
[![respirometr status
badge](https://roaldarbol.r-universe.dev/badges/respirometr)](https://roaldarbol.r-universe.dev)
[![Codecov test
coverage](https://codecov.io/gh/roaldarbol/respirometr/graph/badge.svg)](https://app.codecov.io/gh/roaldarbol/respirometr)
<!-- badges: end -->

*An R toolbox for analysing respirometry data*

The primary aim of the *respirometr* package is to provide a
standardised workflow for analysing respirometry data in a
*tidyverse*-friendly syntax.

## Installation

You can install the development version of *respirometr* with:

| Type | Source | Command |
|----|----|----|
| Development | R-universe | `install.packages("respirometr", repos = "https://roaldarbol.r-universe.dev")` |
| Development | Github | `renv::install("roaldarbol/respirometr")` |

Once you have installed the package, you can load it with:

``` r
library("respirometr")
```

## Documentation

See our docs to go through the workflow, one-by-one:

- [Introduction to
  `respirometr`](https://www.roald-arboel.com/respirometr/articles/respirometr.html)
- [Read respirometry
  data](https://www.roald-arboel.com/respirometr/articles/read-licor.html)
- [Combine with movement data]()

## Status

> **Warning**
>
> 🏗️ The package is currently in early development and the interface is
> subject to change. Feel free to play around and provide feedback.

## Contribute

**If your favourite type of respirometry data or model of respirometer
is not currently supported, we’d love to get a sample of your data to
support it!**

If you enjoy the package, please make sure to [cite it](#citation). If
you find a bug, feel free to open an issue.

<!-- ## Acknowledgements -->

<!-- *animovement* is all about the data, and we are deeply grateful for all those who have shared data with us to implement and test our code. Thank you! -->

<!-- - [Stan Edwards](): Trackball with optical flow, free. -->

<!-- - [Estelle Moubarak](): Trackball with optical flow, fixed. -->

<!-- - [Maria Cozan](): Treadmill with rotary encoder. -->

<!-- - [Violette Chiara](): AnimalTA -->

## Citation

To cite *respirometr* in publications use:

``` r
citation("respirometr")
#> To cite package 'respirometr' in publications use:
#> 
#>   Roald-Arbøl M (2024). "respirometr: An R toolbox for analysing
#>   respirometry data." <http://www.roald-arboel.com/respirometr/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{roaldarbol:2024,
#>     title = {respirometr: An R toolbox for analysing respirometry data},
#>     author = {Mikkel Roald-Arbøl},
#>     year = {2024},
#>     url = {http://www.roald-arboel.com/respirometr/},
#>     abstract = {An R toolbox for analysing respirometry data.},
#>     version = {0.1.0},
#>   }
```
