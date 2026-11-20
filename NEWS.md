# bugsigdbr 

## Changes in version 1.20.0

* `importBugSigDB()` defaults to BugSigDB release v1.4.0
  (10.5281/zenodo.24379934): 11,939 signatures from 1,847 studies, up from
  7,425 signatures from 1,246 studies in v1.3.1.
* New Study columns: `Curation method`, `Curation protocol`,
  `submittedBy`, `submissionDate`, `Review state` and
  `Quality control tag`.
* Releases exclude retracted studies, studies whose review found
  them incorrect, studies with neither a PMID nor a DOI, and
  signatures with no taxa resolved to an NCBI Taxonomy ID.
  `version = "devel"` still includes them; retracted and incorrect
  studies are marked in `Quality control tag` and `Review state`.
* Releases now include experiments without signatures from reviewed
  studies, which record a negative taxonomic result and alpha
  diversity. Their rows in `importBugSigDB()` have `NA` signature
  columns; `getSignatures()` skips them.
* DOIs are normalized to the 10.xxxx/... form. Studies whose DOI
  was written as a doi.org URL were previously left out of
  releases; they are now included.
* Release v1.4.0 uses NCBI taxonomy from ncbi-taxon-db 2026.11.1
  (NCBI taxdump of 1 November 2026).

## Changes in version 1.18.0

* `importBugSigDB()` defaults to BugSigDB release v1.3.1
  (10.5281/zenodo.19740674), the first release filtered by quality
  control: 7,425 reviewed, complete signatures from 1,246 studies.

## Changes in version 1.13.6

* Add domain support for signatures

## Changes in version 1.13.2

* Update functions to accommodate PMID as Study ID

## Changes in version 1.11.4

* Update stable release version for data on Zenodo

## Changes in version 1.8.2

* Uniform first-letter capitalization for Body site and Condition terms

## Changes in version 1.2.0

* `importBugSigDB` accepts Zenodo DOIs and Github hashes to obtain
  defined data releases or `devel` to obtain the latest version
  (new argument `version`).
* Ontology-based queries for experimental factors and body sites
  (new functions `getOntology` and `subsetByOntologies`)
* Compilation of meta-signatures from individual signatures for one
  body site and one condition at a time, weighted by sample size  
  (new function `getMetaSignatures`)

## Version 1.0.0

* released in Bioconductor 3.14!

### New features

* `importBugSigDB`: Obtain published microbial signatures from bugsigdb.org
* `getSignatures`: Obtain microbe signatures from BugSigDB
* `restrictTaxLevel`: Restrict microbe signatures to specific taxonomic levels
* `extractTaxLevel`: Extract specific taxonomic levels from a microbe signature
* `writeGMT`: Write microbe signatures to file in GMT format
* `browseSignature`:  Displaying BugSigDB signatures pages in a web browser
* `browseTaxon`: Displaying BugSigDB taxon pages in a web browser
