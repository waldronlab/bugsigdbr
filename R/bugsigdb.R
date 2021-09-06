#' Obtain published microbial signatures from bugsigdb.org
#' @param cache logical. Should a locally cached version used if available?
#' Defaults to \code{TRUE}.
#' @return a \code{\link{data.frame}}.
#' @references BugSigDB: \url{https://bugsigdb.org}
#' @examples
#'
#'  df <- importBugSigDB()
#'
#' @export
importBugSigDB <- function(cache = TRUE) {
    rname <- "bugsigdb"
    url <- file.path("https://raw.githubusercontent.com/waldronlab",
                     "BugSigDBExports/main/full_dump.tab")

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
    dat <- suppressWarnings(vroom::vroom(from, skip = 1L,
                                         progress = FALSE, show_col_types = FALSE))
    dat <- as.data.frame(dat)
    dat[["MetaPhlAn taxon names"]] <- strsplit(dat[["MetaPhlAn taxon names"]], ",")
    dat[["NCBI Taxonomy IDs"]] <- strsplit(dat[["NCBI Taxonomy IDs"]], ";")
    saveRDS(dat, file = to)
    return(TRUE)
}
