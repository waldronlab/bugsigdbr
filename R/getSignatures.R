#' @name getSignatures
#'
#' @title Obtain microbe signatures from BugSigDB
#' 
#' @description Functionality for obtaining microbe signatures from BugSigDB
#'
#' @param df \code{data.frame} storing BugSigDB data. Typically obtained via
#' \code{\link{importBugSigDB}}.
#' @param tax.id.type Character. Taxonomic ID type of the returned microbe sets.
#' Either \code{"ncbi"} (default), \code{"metaphlan"}, or \code{"taxname"}.
#' @param tax.level character. Either \code{"mixed"} or any subset of 
#' \code{c("kingdom", "phylum", "class", "order", "family", "genus", "species",
#' "strain")}. This full vector is equivalent to \code{"mixed"}.
#' @param exact.tax.level logical. Should only the exact taxonomic level
#' specified by \code{tax.level} be returned? Defaults to \code{TRUE}.
#' If \code{FALSE}, a more general \code{tax.level} is extracted for 
#' microbes given at a more specific taxonomic level.
#' @return a \code{list} of microbe signatures. Each signature is a character 
#' vector of taxonomic IDs depending on the chosen \code{tax.id.type}. 
#' @references BugSigDB: \url{https://bugsigdb.org}
#' @seealso importBugSigDB
#' @examples
#'  df <- importBugSigDB()
#'  sigs <- getSignatures(df)
#' @importFrom utils relist 
#' @export
getSignatures <- function(df,
                           tax.id.type = c("ncbi", "metaphlan", "taxname"),
                           tax.level = "mixed", 
                           exact.tax.level = TRUE)
{
    stopifnot(is.data.frame(df))
    tax.id.type <- match.arg(tax.id.type)
    
    stopifnot(is.character(tax.level))
    if("mixed" %in% tax.level) tax.level <- "mixed" 
    else if(!all(tax.level %in% TAX.LEVELS))
            stop("tax.level must be a subset of { ",
                 paste(TAX.LEVELS, collapse = ", "), " }")

    # extract signatures
    is.study <- grepl("^Study [0-9]+$", df[["Study"]])
    is.exp <- grepl("^Experiment [0-9]+$", df[["Experiment"]])
    df <- df[is.study & is.exp,]
    
    snames <- .makeSigNames(df) 
    sigs <- .extractSigs(df, tax.id.type, tax.level, exact.tax.level)
    names(sigs) <- paste(snames$id, snames$titles, sep = "_")
    sigs
}

#' @name writeGMT
#' 
#' @title Write microbe signatures to file in GMT format
#' 
#' @description Functionality for writing microbe signatures to file in GMT format.
#' 
#' @param sigs A list of microbe signatures (character vectors of taxonomic IDs).
#' @param gmt.file character. Path to output file in GMT format.
#' @return none, writes to file.
#' @references
#' GMT file format:
#' \url{http://www.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats}
#' @export
writeGMT <- function(sigs, gmt.file)
{
  # collapse set members to one tab separated string 
  gs.strings <- vapply(sigs, 
                       function(x) paste(x, collapse = "\t"),
                       character(1))
  
  # paste an not annotated second column (artifact of gmt format)
  ann <- paste(names(sigs), rep(NA, length(sigs)), sep="\t")
  
  # paste all together
  all <- paste(ann, gs.strings, sep="\t")
  
  # collapse all together to a single newline separated string
  all.str <- paste(all, collapse="\n")
  all.str <- paste(all, "\n", sep="")
  
  # write in gmt format
  cat(all.str, file = gmt.file, sep = "")
}

.extractSigs <- function(sigdf, id.type, tax.level, exact.tax.level)
{
    id.col <- ifelse(id.type == "ncbi",
                     "NCBI Taxonomy IDs",
                     "MetaPhlAn taxon names")
    sigs <- sigdf[[id.col]]
    sigs <- strsplit(sigs, ",")
    
    if(tax.level[1] != "mixed")
    {
        if(id.type == "ncbi")
        {
            msigs <- sigdf[["MetaPhlAn taxon names"]]
            msigs <- strsplit(msigs, ",")
        }
        else
        {
          if(!exact.tax.level)
              sigs <- lapply(sigs, .extractTaxLevelSig, tax.level = tax.level)
          msigs <- sigs
        }  

        bugs <- unique(unlist(msigs))
        ind <- lapply(msigs, function(s) match(s, bugs))
        istl <- vapply(bugs, .isTaxLevel, logical(1), tax.level = tax.level)
        subind <- istl[unlist(ind)]
        subind <- relist(subind, ind)
        sigs <- mapply(`[`, sigs, subind)
    }

    if(id.type != "metaphlan")
    {
        sigs <- lapply(sigs, .getTip)
        if(id.type == "taxname")
            sigs <- lapply(sigs, function(s) sub(MPA.REGEXP, "", s))
    }
    sigs
}

.extractTaxLevelSig <- function(sig, tax.level)
    vapply(sig, .extractTaxLevel, character(1), 
           tax.level = tax.level, USE.NAMES = FALSE)

.extractTaxLevel <- function(bug, tax.level)
{
    if(is.na(bug)) return(bug)
    tip <- .getTip(bug)
    tl <- substring(tip, 1, 1)
    ind1 <- match(tl, MPA.TAX.LEVELS)
    ind2 <- match(tax.level, names(MPA.TAX.LEVELS))
    if(ind1 > ind2)
    {
        bug <- unlist(strsplit(bug, "\\|"))
        bug <- paste(bug[seq_len(ind2)], collapse = "|")
    }
    return(bug)
}

.isTaxLevel <- function(s, tax.level)
{
  if(tax.level[1] == "mixed") return(s)
  tip <- .getTip(s)
  tip <- substring(tip, 1, 1)
  mtl <- MPA.TAX.LEVELS[tax.level]
  tip %in% mtl
}

.getTip <- function(n)
{ 
  spl <- strsplit(n, "\\|")
  vapply(spl, function(s) s[length(s)], character(1)) 
}

.makeSigNames <- function(df)
{
    # process experiment information
    eid <- sub("^Experiment ", "", df[["Experiment"]])
    sid <- sub("^Study ", "", df[["Study"]])
    id <- paste(sid, eid, sep = "/")
    
    rel.cols <- c("Condition",
                  "Group 1 name",
                  "Group 0 name")        
    exps <- df[,rel.cols]
    .conc <- function(x) paste(x[1], paste(x[2:3], collapse = "_vs_"), sep = ":")
    einfo <- apply(exps, 1, .conc)
    einfo <- gsub(" ", "-", einfo)
    names(einfo) <- id
  
    # process signature information
    sgid <- sub("^Signature ", "", df[["Signature page name"]])
    up.down <- ifelse(df[["Abundance in Group 1"]] == "increased", "UP", "DOWN")
    titles <- paste(unname(einfo), up.down, sep = "_")
    id <- paste(id, sgid, sep = "/")
    id <- paste("bsdb", id, sep = ":")
    list(id = id, titles = titles)
}

.study2pmid <- function(df)
{
    df <- unique(df[,c("Study", "PMID")])
    s2pmid <- df[["PMID"]]
    names(s2pmid) <- df[["Study"]]
    s2pmid
}

# @param ... Additional arguments for individual microbe set databases.
# For \code{db = "manalyst"}: \itemize{ \item lib: Character. MicrobiomeAnalyst
# taxon set library. Options include taxon sets associated with human genetic
# variations ('gene'), host-intrinsic ('host_int'), host-extrinsic ('host_ext'),
# environmental ('env'), and microbiome-intrinsic ('mic_int') factors.
# TODO: ID mapping
.getMAnalyst <- function(id.type,
                         tax.level,
                         cache, 
                         lib = c("host_int", "host_ext", "env", "mic_int", "gene"))
{
    lib <- match.arg(lib)    

    # cache ?
    msc.name <- paste("mana", lib, id.type, sep = ".")
 
    # should a cached version be used?
    if(cache)
    {
        sigs <- .getResourceFromCache(msc.name)
        if(!is.null(sigs)) return(sigs)
    }
    
    ma.url <- paste0("https://www.microbiomeanalyst.ca/MicrobiomeAnalyst/",
                           "resources/lib/tsea/tsea_")

    if(!(lib %in% c("host_int", "host_ext")))
        lib <- switch(lib, 
                        gene = "host_snps_new",
                        mic_int = "microbiome_int",
                        env = "environment")
    ma.url <- paste0(ma.url, lib, ".csv")
    cont <- readr::read_csv(ma.url)
    cont <- as.data.frame(cont)
    rel.cols <- c("name", "member", "abund_change")
    cont <- cont[,rel.cols]
    
    sigs <- strsplit(cont[["member"]], "; +")
    up.down <- ifelse(cont[["abund_change"]] == "Increase", "UP", "DOWN")
    titles <- sub(" \\(.+\\)$", "", cont[["name"]]) 
    titles <- gsub(" ", "_", titles)   

    id <- seq_along(sigs)
    id <- paste0("MA", id)

    names(sigs) <- paste(id, titles, up.down, sep = "_")
    .cacheResource(sigs, msc.name)
    sigs
}


