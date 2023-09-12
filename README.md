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
library(bugsigdbr)
bsdb <- bugsigdbr::importBugSigDB()
```

## Citation

Ludwig Geistlinger, Chloe Mirzayi, Fatima Zohra, Rimsha Azhar,
Shaimaa Elsafoury, Clare Grieve, Jennifer Wokaty, Samuel David Gamboa-Tuz,
Pratyay Sengupta, Isaac Hecht, Aarthi Ravikrishnan, Rafael S. Goncalves,
Eric Franzosa, Karthik Raman, Vincent Carey, Jennifer B. Dowd,
Heidi E. Jones, Sean Davis, Nicola Segata, Curtis Huttenhower, Levi Waldron (2023)
BugSigDB captures patterns of differential abundance across a broad range of host-associated microbial signatures. 
*Nature Biotechnology*, doi: [10.1038/s41587-023-01872-y](https://doi.org/10.1038/s41587-023-01872-y).
