############################################################
# 
# author: Ludwig Geistlinger
# date: 2021-06-04 13:34:19
# 
# descr: programmatically displaying BugSigDB data in your
#        web browser
# 
############################################################

#' @name browseSignature
#' 
#' @title Displaying BugSigDB signatures pages in a web browser
#' 
#' @description Functionality for programmatically displaying microbe 
#' signatures on BugSigDB signature pages.
#' 
#' @param sname character. Signature name. Expected to start with a prefix
#' of the form \code{"bsdb:<X>/<Y>/<Z>_"} encoding the corresponding
#' BugSigDB signature ID.
#' @return The URL of the selected BugSigDB signature page. If interactive,
#' opens the URL in the default web browser.
#' @references BugSigDB: \url{https://bugsigdb.org}
#' @examples
#' 
#'   sname <- "bsdb:215/1/1_eczema:infant-with-eczema_vs_healthy-control_UP"
#'   browseSignature(sname)
#'   
#' @importFrom utils browseURL
#' @export
browseSignature <- function(sname)
{
    # sanity
    stopifnot(length(sname) == 1 && is.character(sname))
    id <- unlist(strsplit(sname, "_"))[1]
    stopifnot(grepl("^bsdb:", id))
    
    # create the URL
    id <- sub("^bsdb:", "", id)
    id <- unlist(strsplit(id, "/"))
    
    url <- file.path("https://bugsigdb.org",
                     paste0("Study_", id[1]),
                     paste0("Experiment_", id[2]),
                     paste0("Signature_", id[3]))
    
    if(interactive()) browseURL(url)
    return(url)
}

#' @name browseTaxon
#' 
#' @title Displaying BugSigDB taxon pages in a web browser
#' 
#' @description Functionality for programmatically displaying 
#' BugSigDB taxon pages.
#' 
#' @param tax.id character. NCBI taxonomy ID.
#' @return The URL of the selected BugSigDB taxon page. If interactive,
#' opens the URL in the default web browser.
#' @references BugSigDB: \url{https://bugsigdb.org}
#' @examples
#'   
#'   # BugSigDB taxon page for Escherichia coli
#'   browseTaxon("562")
#'   
#' @importFrom utils browseURL
#' @export
browseTaxon <- function(tax.id)
{
    # sanity
    stopifnot(length(tax.id) == 1 && is.character(tax.id))
    stopifnot(grepl("^[0-9]+", tax.id))

    url <- "https://bugsigdb.org/Special:RunQuery/Taxon?Taxon%5BNCBI%5D="
    ext <- "&_run=1"
    url <- paste0(url, tax.id, ext)
    if(interactive()) browseURL(url)
    return(url)
}
