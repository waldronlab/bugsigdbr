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
importBugSigDB <- function(cache = TRUE)
{
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
    sig.cols <- c("MetaPhlAn taxon names", "NCBI Taxonomy IDs")
    for(col in sig.cols) if(!all(is.na(dat[[col]]))) dat[[col]] <- strsplit(dat[[col]], ",")
    saveRDS(dat, file = to)
    return(TRUE)
}
