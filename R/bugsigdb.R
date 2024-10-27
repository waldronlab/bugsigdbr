#' Obtain published microbial signatures from bugsigdb.org
#'
#' @details There are three different options to obtain data from 
#' BugSigDB, as determined by the \code{version} argument.
#' \itemize{
#' \item a Zenodo DOI: use this option if you would like to obtain
#' one of the stable release versions of BugSigDB on Zenodo. These
#' stable release versions of BugSigDB have been automatically checked and
#' manually reviewed and provide for the highest data quality. Select this option
#' if you would like to incorporate BugSigDB into analysis and published
#' research. If not specified otherwise, the \code{importBugSigDB} function
#' will obtain the most recent stable release from Zenodo by default.
#' \item \code{"devel"}: use this option to obtain the latest version
#' ("bleeding edge") of BugSigDB from the BugSigDBExports GitHub repo 
#' (see references).
#' Note that this will also include incomplete and not reviewed content,
#' which should be filtered out prior to an analysis.
#' Select this option if you are a curator that actively contributes to
#' BugSigDB and would like to access data that you and other curators have 
#' recently contributed to BugSigDB and that has not been included in a stable
#' release yet. 
#' \item a git commit hash: it might be occasionally of interest to obtain
#' a specific snapshot of the BugSigDBExports GitHub repo, e.g. for the sake of 
#' debugging and troubleshooting. This can be done by providing the short
#' 7-character git commit hash (SHA) or the full SHA of the export of choice.
#' To provide the full SHA, go to the BugSigDBExports commits page (see references)
#' and use the copy symbol to the left of the 7-character codes to copy the full
#' SHA code of the export version you want to use. 
#'}
#'
#' @param version character. A Zenodo DOI, git commit hash, or "devel".
#' Defaults to the most recent stable release on Zenodo, which includes
#' complete and reviewed content from BugSigDB. See details.
#' @param cache logical. Should a locally cached version used if available?
#' Defaults to \code{TRUE}.
#'
#' @return a \code{\link{data.frame}}.
#' @references BugSigDB: \url{https://bugsigdb.org}
#'
#' Stable release: \url{https://doi.org/10.5281/zenodo.10627578}
#'
#' Latest version (incl. not reviewed content): 
#'      \url{https://github.com/waldronlab/BugSigDBExports}
#'
#' Release v1.2.2: \url{https://zenodo.org/records/13997429}
#'
#' Release v1.2.1: \url{https://zenodo.org/records/10627578}
#'
#' Release v1.2.0: \url{https://zenodo.org/records/10407666}
#'
#' Release v1.1.0: \url{https://zenodo.org/records/6468009}
#'
#' Release v1.0.2: \url{https://zenodo.org/records/5904281}
#'
#' Release v1.0.1: \url{https://zenodo.org/records/5819260}
#'
#' Release v1.0.0: \url{https://zenodo.org/records/5606166}
#'
#' BugSigDBExports commits page:
#'      \url{https://github.com/waldronlab/BugSigDBExports/commits/devel}
#' @examples
#'
#'  df <- importBugSigDB()
#'
#' @export
importBugSigDB <- function(version = "10.5281/zenodo.13997429", cache = TRUE)
{
    version <- tolower(version)

    if(grepl("^10.5281/zenodo\\.", version))
    {
        version <- sub("^10.5281/zenodo\\.", "", version)
        url <- paste0("https://zenodo.org/record/", version, "/files/full_dump.csv")
    } 
    else if(version == "devel" || grepl("^[0-9a-z]{7}", version))
    {
        if(version != "devel") version <- substring(version, 1, 7)
        url <- paste0("https://raw.githubusercontent.com/waldronlab/",
                      "BugSigDBExports/", version, "/full_dump.csv")
    } 
    else stop("Version ", version, " does not exist. The version must be a",
              " DOI such as '10.5281/zenodo.5819260', a Github hash",
              " such as '30383a9', or 'devel'.")

    # should a cache version be used?
    rname <- paste("bugsigdb", version, sep = "-")
    if(cache) bsdb.file <- .getResourceFromCache(rname, FUN = .getdf)
    if(!cache || is.null(bsdb.file))
    {
        bsdb.file <- .cacheResource(rname, url, download = FALSE, ext = ".rds")
        bsdb.file <- suppressMessages(.getResourceFromCache(rname, FUN = .getdf))
    }
    bsdb <- readRDS(bsdb.file)
    return(bsdb)
}

.getdf <- function(from, to)
{
    dat <- suppressWarnings(vroom::vroom(from, skip = 1L,
                                         altrep = FALSE, 
                                         progress = FALSE,
                                         show_col_types = FALSE))
    dat <- as.data.frame(dat)
    dat[["MetaPhlAn taxon names"]] <- strsplit(dat[["MetaPhlAn taxon names"]], ",")
    dat[["NCBI Taxonomy IDs"]] <- strsplit(dat[["NCBI Taxonomy IDs"]], ";")

    .rmEmpty <- function(x) x[x != ""] 
    dat[["MetaPhlAn taxon names"]] <- lapply(dat[["MetaPhlAn taxon names"]], .rmEmpty)
    dat[["NCBI Taxonomy IDs"]] <- lapply(dat[["NCBI Taxonomy IDs"]], .rmEmpty)

    saveRDS(dat, file = to)
    return(TRUE)
}
