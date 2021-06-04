############################################################
# 
# author: Ludwig Geistlinger
# date: 2021-06-04 13:34:19
# 
# descr: programmatically displaying BugSigDB data in your
#        web browser
# 
############################################################

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
    else return(url)
}

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
    else return(url)
}
