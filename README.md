# bugsigdbr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check-bioc](https://github.com/waldronlab/bugsigdbr/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/waldronlab/bugsigdbr/actions)
[![Codecov test
coverage](https://codecov.io/gh/waldronlab/bugsigdbr/branch/main/graph/badge.svg)](https://codecov.io/gh/waldronlab/bugsigdbr?branch=main)
<!-- badges: end -->

Utilities for accessing data from [BugSigDB.org](https://bugsigdb.org) and creating plain text
signatures.

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `bugsigdbr` from
[Bioconductor](https://bioconductor.org/packages/bugsigdbr/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("bugsigdbr")
```

Or the development version from
[GitHub](https://github.com/waldronlab/bugsigdbr) with:

``` r
BiocManager::install("waldronlab/bugsigdbr")
```

## Example

``` r
library("bugsigdbr")

bsdb <- bugsigdbr::importBugSigDB()
```

## Citation

Below is the citation output from using `citation('bugsigdbr')` in R.
Please run this yourself to check for any updates on how to cite
**bugsigdbr**.

``` r
print(citation('bugsigdbr'), bibtex = TRUE)
```
    ## 
    ## To cite package 'bugsigdbr' in publications use:
    ## 
    ##   Ludwig Geistlinger and Levi Waldron (NA). bugsigdbr: R-side access to
    ##   published microbial signatures from BugSigDB. R package version
    ##   0.99.0. https://github.com/waldronlab/bugsigdbr
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {bugsigdbr: R-side access to published microbial signatures from BugSigDB},
    ##     author = {Ludwig Geistlinger and Levi Waldron},
    ##     note = {R package version 0.99.0},
    ##     url = {https://github.com/waldronlab/bugsigdbr},
    ##   }

Please note that the `bugsigdbr` was only made possible thanks to many
other R and bioinformatics software authors, which are cited either in
the vignettes and/or the paper(s) describing this package.

## Code of Conduct

Please note that the `bugsigdbr` project is released with a [Contributor
Code of Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.13/biocthis)*.
