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
#' @param min.size integer. Minimum signature size. Defaults to 1, which will
#' filter out empty signature. Use \code{min.size = 0} to keep empty signatures.
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
                          exact.tax.level = TRUE,
                          min.size = 1)
{
    stopifnot(is.data.frame(df))
    tax.id.type <- match.arg(tax.id.type)

    stopifnot(is.character(tax.level))
    if ("mixed" %in% tax.level)
        tax.level <- "mixed" else if (!all(tax.level %in% TAX.LEVELS))
        stop("tax.level must be a subset of { ",
             paste(TAX.LEVELS, collapse = ", "),
             " }")

    is.spec.tax.level <- length(tax.level) == 1 && tax.level != "mixed" 
    if(!exact.tax.level && !is.spec.tax.level)
        stop("Using exact.tax.level = FALSE requires ",
             "to select a specific taxonomic level")

    # rm NA signatures
    nna <- !is.na(df[["MetaPhlAn taxon names"]])
    df <- df[nna,]

    # extract signatures
    is.study <- grepl("^Study [0-9]+$", df[["Study"]])
    is.exp <- grepl("^Experiment [0-9]+$", df[["Experiment"]])
    df <- df[is.study & is.exp, ]
    
    snames <- .makeSigNames(df)
    sigs <- .extractSigs(df, tax.id.type, tax.level, exact.tax.level)
    names(sigs) <- paste(snames$id, snames$titles, sep = "_")
    sigs <- lapply(sigs, unique)
    sigs <- sigs[lengths(sigs) >= min.size]
    return(sigs)
}

#' @name getMetaSignatures
#'
#' @title Obtain meta-signatures for a column of interest
#'
#' @description Functionality for obtaining meta-signatures for a column of 
#' interest
#'
#' @param df \code{data.frame} storing BugSigDB data. Typically obtained via
#' \code{\link{importBugSigDB}}.
#' @param column character. Column of interest. Need to be a valid column name
#' of \code{df}.
#' @param direction character. Indicates direction of abundance change for signatures
#' to be included in the computation of meta-signatures. Use \code{"UP"} to restrict
#' computation to signatures with increased abundance in the exposed group. Use 
#' \code{"DOWN"} to restrict to signatures with decreased abundance in the exposed
#' group. Defaults to \code{"BOTH"} which will not filter signatures by direction
#' of abundance change.
#' @param min.studies integer. Minimum number of studies for a category in \code{column}
#' to be included. Defaults to 2, which will then only compute meta-signatures for
#' categories investigated by at least two studies. 
#' @param min.taxa integer. Minimum size for meta-signatures. Defaults to 5, which
#' will then only include meta-signatures containing at least 5 taxa.
#' @param comb.fun function. Function for combining sample size of the exposed group
#' and sample size of the unexposed group into an overall study sample size. Defaults
#' to \code{sum} which will simply add sample sizes of exposed and unexposed group.
#' @param ... additionals argument passed on to \code{\link{getSignatures}}.
#' @return A \code{list} of meta-signatures, each meta-signature being a named
#' numeric vector. Names are the taxa of the meta-signature, numeric values 
#' correspond to sample size weights associated with each taxon.
#' @seealso getSignatures
#' @examples
#'  df <- importBugSigDB()
#'
#'  # Body-site specific meta-signatures composed from signatures reported as both 
#'  # increased or decreased across all conditions of study:
#'  bs.meta.sigs <- getMetaSignatures(df, column = "Body site")
#'
#'  # Condition-specific meta-signatures from fecal samples, increased
#'  # in conditions of study. Use taxonomic names instead of the default NCBI IDs:
#'  df.feces <- df[df$`Body site` == "Feces", ]
#'  cond.meta.sigs <- getMetaSignatures(df.feces, column = "Condition", 
#'                                      direction = "UP", tax.id.type = "taxname")
#'
#'  # Inspect the results
#'  names(cond.meta.sigs)
#'  cond.meta.sigs["Bipolar disorder"]
#' @export
getMetaSignatures <- function(df, 
                              column,
                              direction = c("BOTH", "UP", "DOWN"),   
                              min.studies = 2,
                              min.taxa = 5,
                              comb.fun = sum,  
                              ...)
{
    # check and restrict by column of choice
    if(!(column %in% colnames(df))) 
        stop("<column> must be a valid colname of <df>")
    ind <- !is.na(df[[column]]) & !grepl(",", df[[column]])
    df <- df[ind,]

    # restrict by direction of abundance change
    direction <- match.arg(direction)
    if(direction != "BOTH")
    {
        direction <- ifelse(direction == "UP", "increased", "decreased")
        df <- subset(df, `Abundance in Group 1` == direction)
    }

    # include only categories with defined min number of studies
    spl <- split(df$PMID, df[[column]])
    spl <- lapply(spl, unique)
    lens <- lengths(spl)
    names(lens) <- names(spl)
    lens <- sort(lens, decreasing = TRUE)
    incl <- names(lens)[lens >= min.studies]
    ind <- df[[column]] %in% incl
    df <- df[ind,]

    # obtain and group signatures and sample size by 
    sigs <- getSignatures(df, min.size = 0, ...)            
    ss.cols <- paste("Group", c(0,1), "sample size")
    ss <- apply(df[,ss.cols], 1, comb.fun)
    nna <- !is.na(ss)
    ss <- ss[nna]
    sigs <- sigs[nna]
    df <- df[nna,]
    spl <- split(sigs, df[[column]])
    spl.ss <- split(ss, df[[column]])    

    # compute weights based on sample size
    ## here we use a simple voting approach, we sum sample sizes for one 
    ## taxon at a time, and then divide by the total sample size to obtain weights 
    spl.ssr <- lapply(names(spl.ss), function(s) rep(unname(spl.ss[[s]]), 
                                                     lengths(spl[[s]])))
    names(spl.ssr) <- names(spl.ss) 
    for(i in seq_along(spl.ssr)) names(spl.ssr[[i]]) <- unlist(spl[[i]])
    .tsum <- function(x) tapply(x, names(x), sum) 
    spl.ssr <- lapply(spl.ssr, .tsum)
    spl.ssr <- spl.ssr[lengths(spl.ssr) >= min.taxa]
    sums <- vapply(spl.ssr, sum, numeric(1))
    for(i in seq_along(spl.ssr)) spl.ssr[[i]] <- spl.ssr[[i]] / sums[i]
    spl.ssr <- lapply(spl.ssr, sort, decreasing = TRUE) 
    return(spl.ssr) 
}


#' @name restrictTaxLevel
#'
#' @title Restrict microbe signatures to specific taxonomic levels
#'
#' @description Functionality for restricting microbe signatures to specific
#' taxonomic levels such as genus and species.
#'
#' @param df \code{data.frame} storing BugSigDB data. Typically obtained via
#' \code{\link{importBugSigDB}}.
#' @param tax.level character. Either \code{"mixed"} or any subset of
#' \code{c("kingdom", "phylum", "class", "order", "family", "genus", "species",
#' "strain")}. This full vector is equivalent to \code{"mixed"}.
#' @param exact.tax.level logical. Should only the exact taxonomic level
#' specified by \code{tax.level} be returned? Defaults to \code{TRUE}.
#' If \code{FALSE}, a more general \code{tax.level} is extracted for
#' microbes given at a more specific taxonomic level.
#' @param min.size integer. Minimum signature size. Defaults to 1, which will
#' filter out empty signatures. Use \code{min.size = 0} to keep empty
#' signatures.
#' @return a \code{data.frame} with microbe signature columns restricted to
#' chosen
#' taxonomic level(s).
#' @references BugSigDB: \url{https://bugsigdb.org}
#' @seealso importBugSigDB
#' @examples
#'  df <- importBugSigDB()
#'  df <- restrictTaxLevel(df, tax.level = "genus")
#' @export
restrictTaxLevel <- function(df,
                             tax.level = "mixed",
                             exact.tax.level = TRUE,
                             min.size = 1) {
    stopifnot(is.data.frame(df))
    stopifnot(is.character(tax.level))
    if ("mixed" %in% tax.level)
        tax.level <- "mixed"
    else if (!all(tax.level %in% TAX.LEVELS))
        stop("tax.level must be a subset of { ",
             paste(TAX.LEVELS, collapse = ", "),
             " }")

    # rm NA signatures
    nna <- !is.na(df[["MetaPhlAn taxon names"]])
    df <- df[nna,]

    # extract signatures
    is.study <- grepl("^Study [0-9]+$", df[["Study"]])
    is.exp <- grepl("^Experiment [0-9]+$", df[["Experiment"]])
    df <- df[is.study & is.exp, ]

    df[["NCBI Taxonomy IDs"]] <- .extractSigs(df,
                                              tax.id.type = "ncbi",
                                              tax.level,
                                              exact.tax.level)
    df[["MetaPhlAn taxon names"]] <- .extractSigs(df,
                                                  tax.id.type = "metaphlan",
                                                  tax.level,
                                                  exact.tax.level)
    df[["NCBI Taxonomy IDs"]] <- lapply(df[["NCBI Taxonomy IDs"]], unique)
    df[["MetaPhlAn taxon names"]] <- lapply(df[["MetaPhlAn taxon names"]],
                                            unique)
    if (min.size) {
        ind <- lengths(df[["MetaPhlAn taxon names"]]) > min.size - 1
        df <- df[ind, ]
    }
    return(df)
}

#' @name extractTaxLevel
#'
#' @title Extract specific taxonomic levels from a microbe signature
#'
#' @description Functionality for extracting specific taxonomic levels
#' (such as genus and species) from a microbe signature containing taxonomic
#' clades in MetaPhlAn format.
#'
#' @param sig character. Microbe signature containing taxonomic
#' clades in MetaPhlAn format.
#' @param tax.id.type Character. Taxonomic ID type of the returned microbe
#' sets.
#' Either \code{"metaphlan"} (default) or \code{"taxname"}.
#' @param tax.level character. Either \code{"mixed"} or any subset of
#' \code{c("kingdom", "phylum", "class", "order", "family", "genus", "species",
#' "strain")}. This full vector is equivalent to \code{"mixed"}.
#' @param exact.tax.level logical. Should only the exact taxonomic level
#' specified by \code{tax.level} be returned? Defaults to \code{TRUE}.
#' If \code{FALSE}, a more general \code{tax.level} is extracted for
#' microbes given at a more specific taxonomic level.
#' @return a character vector storing taxonomic clades restricted to
#' chosen taxonomic level(s).
#' @references BugSigDB: \url{https://bugsigdb.org}
#' @examples
#'
#'  ord <- "k__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales"
#'  sig <- c("f__Lactobacillaceae|g__Lactobacillus",
#'           "f__Aerococcaceae|g__Abiotrophia|s__Abiotrophia defectiva",
#'           "f__Lactobacillaceae|g__Limosilactobacillus|s__Limosilactobacillus mucosae")
#'  sig <- paste(ord, sig, sep = "|")
#'  sig <- extractTaxLevel(sig, tax.level = "genus")
#'  sig <- extractTaxLevel(sig, tax.level = "genus", exact.tax.level = FALSE)
#'  sig <- extractTaxLevel(sig,
#'                         tax.id.type = "taxname",
#'                         tax.level = "genus",
#'                         exact.tax.level = FALSE)
#'
#' @export
extractTaxLevel <- function(sig,
                            tax.id.type = c("metaphlan", "taxname"),
                            tax.level = "mixed",
                            exact.tax.level = TRUE) {
    stopifnot(is.character(sig))
    stopifnot(is.character(tax.level))
    tax.id.type <- match.arg(tax.id.type)
    if ("mixed" %in% tax.level)
        tax.level <- "mixed"
    else if (!all(tax.level %in% TAX.LEVELS))
        stop("tax.level must be a subset of { ",
             paste(TAX.LEVELS, collapse = ", "),
             " }")

    if (tax.level[1] != "mixed") {
        if (!exact.tax.level)
            sig <- .extractTaxLevelSig(sig, tax.level = tax.level)
        istl <- vapply(sig, .isTaxLevel, logical(1), tax.level = tax.level)
        sig <- sig[istl]
    }

    if (tax.id.type != "metaphlan") {
        sig <- .getTip(sig)
        sig <- sub(MPA.REGEXP, "", sig)
    }
    return(sig)
}

#' @name writeGMT
#'
#' @title Write microbe signatures to file in GMT format
#'
#' @description Functionality for writing microbe signatures to file in GMT
#' format.
#'
#' @param sigs A list of microbe signatures (character vectors of taxonomic
#' IDs).
#' @param gmt.file character. Path to output file in GMT format.
#' @param ... Arguments passed on to cat()
#' @return none, writes to file.
#' @references
#' GMT file format:
#' \url{http://www.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats}
#' @examples
#' bsdb <- importBugSigDB()
#' sigs <- getSignatures(bsdb)
#' writeGMT(sigs, gmt.file = "signatures.gmt")
#' file.remove("signatures.gmt") 
#' @export
writeGMT <- function(sigs, gmt.file, ...) {
    # collapse set members to one tab separated string
    gs.strings <- vapply(sigs,
                         function(x) paste(x, collapse = "\t"),
                         character(1))

    # paste an not annotated second column (artifact of gmt format)
    ann <- paste(names(sigs), rep(NA, length(sigs)), sep = "\t")

    # paste all together
    all <- paste(ann, gs.strings, sep = "\t")

    # collapse all together to a single newline separated string
    all.str <- paste(all, collapse = "\n")
    all.str <- paste(all, "\n", sep = "")

    # write in gmt format
    cat(all.str, file = gmt.file, sep = "", ...)
}

.extractSigs <- function(sigdf, tax.id.type, tax.level, exact.tax.level)
{
    id.col <- ifelse(tax.id.type == "ncbi",
                     "NCBI Taxonomy IDs",
                     "MetaPhlAn taxon names")
    sigs <- sigdf[[id.col]]

    if (tax.level[1] != "mixed")
    {
        if (tax.id.type == "ncbi")
            sigs <- .addPrefix(sigs, sigdf[["MetaPhlAn taxon names"]])
        if (!exact.tax.level)
            sigs <- lapply(sigs, .extractTaxLevelSig, tax.level = tax.level)

        bugs <- unique(unlist(sigs))
        ind <- lapply(sigs, function(s) match(s, bugs))
        istl <- vapply(bugs, .isTaxLevel, logical(1), tax.level = tax.level)
        subind <- istl[unlist(ind)]
        subind <- relist(subind, ind)
        sigs <- mapply(`[`, sigs, subind)
        if(is.matrix(sigs)) sigs <- as.list(data.frame(sigs))
    }

    if (tax.id.type != "metaphlan") {
        sigs <- lapply(sigs, .getTip)
        sigs <- lapply(sigs, function(s) sub(MPA.REGEXP, "", s))
    }

    return(sigs)
}

.addPrefix <- function(sigs, msigs)
{
    s <- unlist(sigs)
    m <- unlist(msigs)
    s <- strsplit(s, "\\|")
    m <- strsplit(m, "\\|")
    sl <- unlist(s)
    msl <- unlist(m)
    msl <- substring(msl, 1, 3)
    sl <- paste0(msl, sl)     
    s <- relist(sl, s)   
    s <- lapply(s, paste, collapse = "|")
    s <- unlist(s)
    s <- relist(s, sigs)
}

.extractTaxLevelSig <- function(sig, tax.level)
{
    vapply(sig,
           .extractTaxLevel,
           character(1),
           tax.level = tax.level, USE.NAMES = FALSE)
}

.extractTaxLevel <- function(bug, tax.level)
{
    if (is.na(bug))
        return(bug)
    tip <- .getTip(bug)
    tl <- substring(tip, 1, 1)
    ind1 <- match(tl, MPA.TAX.LEVELS)
    ind2 <- match(tax.level, names(MPA.TAX.LEVELS))
    if (ind1 > ind2) {
        bug <- unlist(strsplit(bug, "\\|"))
        bug <- paste(bug[seq_len(ind2)], collapse = "|")
    }
    return(bug)
}

.isTaxLevel <- function(s, tax.level)
{
    if (tax.level[1] == "mixed")
        return(s)
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

    rel.cols <- c("Condition", "Group 1 name", "Group 0 name")
    exps <- df[, rel.cols]
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
    df <- unique(df[, c("Study", "PMID")])
    s2pmid <- df[["PMID"]]
    names(s2pmid) <- df[["Study"]]
    s2pmid
}

# @param ... Additional arguments for individual microbe set databases.  For
# \code{db = "manalyst"}: \itemize{ \item lib: Character. MicrobiomeAnalyst
# taxon set library. Options include taxon sets associated with human genetic
# variations ('gene'), host-intrinsic ('host_int'), host-extrinsic
# ('host_ext'), environmental ('env'), and microbiome-intrinsic ('mic_int')
# factors.  TODO: ID mapping
.getMAnalyst <- function(tax.id.type,
                         tax.level,
                         cache,
                         lib = c("host_int", "host_ext",
                                 "env", "mic_int", "gene"))
{
    lib <- match.arg(lib)

    # cache ?
    msc.name <- paste("mana", lib, tax.id.type, sep = ".")

    # should a cached version be used?
    if (cache) {
        sigs <- .getResourceFromCache(msc.name)
        if (!is.null(sigs))
            return(sigs)
    }

    ma.url <- paste0("https://www.microbiomeanalyst.ca/MicrobiomeAnalyst/",
                     "resources/lib/tsea/tsea_")

    if (!(lib %in% c("host_int", "host_ext")))
        lib <- switch(lib,
                      gene = "host_snps_new",
                      mic_int = "microbiome_int",
                      env = "environment")
    ma.url <- paste0(ma.url, lib, ".csv")
    cont <- vroom::vroom(ma.url)
    cont <- as.data.frame(cont)
    rel.cols <- c("name", "member", "abund_change")
    cont <- cont[, rel.cols]

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

