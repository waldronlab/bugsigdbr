#' Obtain published microbial signatures from bugsigdb.org
#' @param version character. A Zenodo DOI, git commit hash, or "devel".
#' Defaults to the most recent stable release on Zenodo, which includes
#' complete and reviewed content from BugSigDB.
#' @param cache logical. Should a locally cached version used if available?
#' Defaults to \code{TRUE}.
#'
#' @return a \code{\link{data.frame}}.
#' @references BugSigDB: \url{https://bugsigdb.org}
#'
#' Stable release: \url{https://doi.org/10.5281/zenodo.5606165}
#'
#' Latest version (incl. not reviewed content): 
#'      \url{https://github.com/waldronlab/BugSigDBExports}
#' @examples
#'
#'  df <- importBugSigDB()
#'
#' @export
importBugSigDB <- function(version = "10.5281/zenodo.5819260", cache = TRUE) 
{
    version <- tolower(version)

    if(grepl("^10.5281/zenodo\\.", version))
    {
        version <- sub("^10.5281/zenodo\\.", "", version)
        url <- paste0("https://zenodo.org/record/", version, "/files/full_dump.csv")
    } 
    else if(version == "devel" || grepl("^[0-9a-z]{7}$", version))
    {
        if(version == "devel") version = "main"
        url <- paste0("https://raw.githubusercontent.com/waldronlab/",
                      "BugSigDBExports/", version, "/full_dump.csv")
    } 
    else stop("Version ", version, " does not exist. The version must be a",
              " DOI, such as '10.5281/zenodo.5819260', a Github hash,",
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
    dat <- suppressWarnings(vroom::vroom(from, skip = 1L, progress = FALSE, show_col_types = FALSE))
    dat <- as.data.frame(dat)
    dat[["MetaPhlAn taxon names"]] <- strsplit(dat[["MetaPhlAn taxon names"]], ",")
    dat[["NCBI Taxonomy IDs"]] <- strsplit(dat[["NCBI Taxonomy IDs"]], ";")
    saveRDS(dat, file = to)
    return(TRUE)
}
