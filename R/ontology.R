############################################################
# 
# author: Ludwig Geistlinger
# date: 2021-12-09 12:23:38
# 
# descr: ontology-based queries for experimental factors and
#        body sites
# 
############################################################

#' Ontology-based subsetting of BugSigDB signatures 
#' @description This function facilitates ontology-based queries for experimental
#' factors and body sites. 
#' @param df \code{data.frame} storing BugSigDB data. Typically obtained via
#' \code{\link{importBugSigDB}}.
#' @param column character. Column of \code{df} on which subsetting should be
#' performed. Should be either \code{"Body site"} (default) or \code{"Condition"}.
#' @param term character. A valid term of \code{ontology}. Subsetting by this term
#' then involves subsetting \code{column} to all descendants of that term in the
#' the chosen \code{ontology} and that are present in the chosen \code{column} of
#' \code{df}. 
#' @param ontology an object of class \code{ontology_index} as defined in the
#' ontologyIndex package. Typically obtained via \code{\link{getOntology}}.
#' @return a \code{data.frame} with the chosen column restricted to descendants
#' of the chosen term in the chosen ontology.
#' @seealso \code{importBugSigDB}, \code{getOntology} 
#' @references 
#'  EFO: \url{https://www.ebi.ac.uk/ols/ontologies/efo}
#'
#'  UBERON: \url{https://www.ebi.ac.uk/ols/ontologies/uberon}
#' @examples
#'
#'  # (1) Obtain BugSigDB data
#'  df <- importBugSigDB()
#'
#'  # (2) Obtain ontology of interest as an R object
#'  uberon <- getOntology("uberon")
#'
#'  # (3) High-level query on body site
#'  sdf <- subsetByOntology(df, 
#'                          column = "Body site",
#'                          term = "digestive system element",
#'                          ontology = uberon)
#'  table(sdf[,"Body site"])
#'
#' @importFrom methods is
#' @export
subsetByOntology <- function(df, 
                             column = c("Body site", "Condition"),
                             term, 
                             ontology)
{
    
    if(!requireNamespace("ontologyIndex"))
        stop("Please install the 'ontologyIndex' package to use 'subsetByOntology'")
    
    # sanity
    stopifnot(is.data.frame(df))
    column <- match.arg(column)
    stopifnot(column %in% colnames(df))
    stopifnot(is(ontology, "ontology_index"))
    if(!(term %in% ontology$name)) stop("Invalid ontology term: ", term)

    # get mapping: term -> ID
    id.column <- ifelse(column == "Condition", "EFO ID", "UBERON ID")    
    ids <- strsplit(df[,id.column], ",")

    # get ancestors for each term, ... 
    # ... and check whether term of interest is among them
    ancs <- lapply(ids, function(i) ontology$name[unlist(ontology$ancestors[i])])
    ind <- vapply(ancs, function(a) term %in% a, logical(1))

    return(df[ind,])
}

#' Obtain the EFO and UBERON ontology
#' @description Lightweight wrapper around \code{ontologyIndex::get_ontology}
#' to parse the Experimental Factor Ontology (EFO) or the Uber-anatomy ontology
#' (UBERON) from OBO format into an R object.
#' @param onto character. Ontology to obtain. Should be either \code{"efo"} (default)
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

    if(!requireNamespace("ontologyIndex"))
        stop("Please install the 'ontologyIndex' package to use 'getOntology'")

    rels <- "is_a"
    if(oname == "uberon") rels <- c(rels, "part_of") 
    .getonto <- function(from, to)
    {
        onto <- ontologyIndex::get_ontology(from, 
                                        propagate_relationships = rels)
        saveRDS(onto, file = to)
        return(TRUE)
    }

    if(cache) onto.file <- .getResourceFromCache(oname, FUN = .getonto)
    if(!cache || is.null(onto.file))
    { 
        onto.file <- .cacheResource(oname, onto.url, download = FALSE, ext = ".rds")
        onto.file <- suppressMessages(.getResourceFromCache(oname, FUN = .getonto))
    }   
    onto <- readRDS(onto.file) 
    return(onto)
}
