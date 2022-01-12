#' Obtain published microbial signatures from bugsigdb.org
#' @param cache logical. Should a locally cached version used if available?
#' @param version character. A Zenodo DOI, git commit hash, or "devel". Defaults
#'     to the most recent release on Zenodo, data directly from BugSigDB.
#'
#' Defaults to \code{TRUE}.
#' @return a \code{\link{data.frame}}.
#' @references BugSigDB: \url{https://bugsigdb.org}
#' @examples
#'
#'  df <- importBugSigDB()
#'
#' @export
importBugSigDB <- function(cache = TRUE, version = "10.5281/zenodo.5819260") {

    version <- tolower(version)

    if (grepl("^10.5281/zenodo\\.", version)) {
        version <- sub("^10.5281/zenodo\\.", "", version)
        url <- paste0("https://zenodo.org/record/", version,
                      "/files/full_dump.csv")
        rname <- paste("bugsigdb-zenodo", version, sep = "-")
    } else if (version == "devel") {
        url <- "https://tinyurl.com/3nvzm3fx"
        rname <- paste("bugsigdb", version, sep = "-")
    } else if (grepl("^[0-9a-z]{7}$", version)) {
        url <- paste0("https://raw.githubusercontent.com/waldronlab/BugSigDBExports/",
                      version, "/full_dump.csv")
        rname <- paste("bugsigdb-github", version, sep = "-")
    } else {
        stop(paste("Version", version, "does not exist. The version must be a",
                   "DOI, such as '10.5281/zenodo.5819260', a Github hash,",
                   "such as '30383a9', or 'devel'.", sep = " "))
    }

    # should a cache version be used?
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
