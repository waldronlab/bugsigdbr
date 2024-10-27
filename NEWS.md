# bugsigdbr 

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
