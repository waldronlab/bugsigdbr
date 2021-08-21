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

    # should a cache version be used?        
    if(cache)
    {
        bugsigdb <- .getResourceFromCache(rname)
        if(!is.null(bugsigdb)) return(bugsigdb) 
    }
     
    # pull the data
    sigs <- readr::read_csv("https://tinyurl.com/yakgsowm")
    exps <- readr::read_csv("https://tinyurl.com/yb2fmpa3")
    studs <- readr::read_csv("https://tinyurl.com/ycg8fs9x")

    # merge the data
    ind <- colnames(exps) == "Experiment page name"
    colnames(exps)[ind] <- "Experiment"
    ind <- colnames(studs) == "Study page name"
    colnames(studs)[ind] <- "Study"
    sig.exp <- plyr::join(exps, sigs, by = c("Study", "Experiment"))
    bugsigdb <- plyr::join(studs, sig.exp, by = "Study")

    sig.cols <- c("MetaPhlAn taxon names", "NCBI Taxonomy IDs")
    for(col in sig.cols) bugsigdb[[col]] <- strsplit(bugsigdb[[col]], ",")

    .cacheResource(bugsigdb, rname)
    return(bugsigdb)
}
