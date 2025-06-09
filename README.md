

# citer: Boosting your R packages’ citations

Although used a core of scientific endeavors, citations of scientific
software are often overlooked. This package provides tools to boost
citation of R packages, making it easier for users to cite the software
they use in their research.

## Installation

Using `remotes`:

``` r
remotes::install_github("UofUEpiBio/citer")
```

## Usage

Setting up citations via `.onLoad()`:

``` r
citer::cite_on_load()
```

Setting up citations on a function call:

``` r
citer::cite_on_call()
```

## Code of Conduct

Please note that the citer project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
