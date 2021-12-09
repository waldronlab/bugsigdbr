############################################################
# 
# author: Ludwig Geistlinger
# date: 2021-12-09 12:23:38
# 
# descr: ontology-based queries for experimental factors and
#        body sites
# 
############################################################

#' Obtain the EFO and UBERON ontology
#' @description Lightweight wrapper around \code{ontologyIndex::get_ontology}
#' to parse the Experimental Factor Ontology (EFO) or the Uber-anatomy ontology
#' (UBERON) from OBO format into an R object.
#' @param onto character. Ontology to obtain. Should be either \code{"efo"}
#' to obtain the Experimental Factor Ontology (EFO) or \code{"uberon"} to obtain
#' the Uber-anatomy ontology (UBERON).   
#' @param cache logical. Should a locally cached version used if available?
#' Defaults to \code{TRUE}.
#' @return An object of class \code{ontology_index} as defined in the
#' ontologyIndex package.
#' @seealso \code{get_ontology} from the ontologyIndex package.
#' @references 
#'  EFO: \url{https://www.ebi.ac.uk/ols/ontologies/efo}
#'
#'  UBERON: \url{https://www.ebi.ac.uk/ols/ontologies/uberon}
#' @examples
#'
#'  uberon <- getOntology("uberon")
#'
#' @export
getOntology <- function(onto = c("efo", "uberon"), 
                        cache = TRUE)
{
    oname <- match.arg(onto)
    onto.url <- ifelse(oname == "efo", EFO.OBO.URL, UBERON.OBO.URL)

    if(cache) onto.file <- .getResourceFromCache(oname, FUN = .getonto)
    if(!cache || is.null(onto.file))
    { 
        if(!requireNamespace("ontologyIndex"))
            stop("Please install the 'ontologyIndex' package to use 'getOntology'")
  
        onto.file <- .cacheResource(oname, onto.url, download = FALSE, ext = ".rds")
        onto.file <- suppressMessages(.getResourceFromCache(oname, FUN = .getonto))
    }   
    onto <- readRDS(onto.file) 
    return(onto)
}

.getonto <- function(from, to)
{
    rels <- "is_a"
    if(grepl("uberon", from)) rels <- c(rels, "part_of") 
    onto <- ontologyIndex::get_ontology(from, 
                                        propagate_relationships = rels)
    saveRDS(onto, file = to)
    return(TRUE)
}
