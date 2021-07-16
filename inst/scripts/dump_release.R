############################################################
# 
# author: Ludwig Geistlinger
# date: 2021-07-13 19:49:35
# 
# descr: dump all files associated with a BugSigDB release
#        into a specified folder
#
# call: Rscript dump_release.R <version> <output.directory> 
#
############################################################

library(bugsigdbr)
library(readr)

# command line arguments
cmd.args <- commandArgs(trailingOnly = TRUE)
if(length(cmd.args) != 2) 
    stop("Usage: Rscript dump_release.R <version> <output.directory>")
version <- cmd.args[1]
out.dir <- cmd.args[2]
stopifnot(file.exists(out.dir))

# header line for output files
header <- paste0("# BugSigDB ", version, 
                 ", License: Creative Commons Attribution 4.0 International",
                 ", URL: https://bugsigdb.org\n")

# import 
bsdb <- bugsigdbr::importBugSigDB()
abstr.col <- "Abstract"
bsdb <- bsdb[,colnames(bsdb) != abstr.col]

#TODO: restrict to reviewed content

# write full dump
tab.file <- file.path(out.dir, "full_dump.tab")
cat(header, file = tab.file)
readr::write_tsv(bsdb, file = tab.file, append = TRUE, col_names = TRUE)

# helper function to add a header line to an already written GMT file
addHeader <- function(header, out.file)
{
    fconn <- file(out.file, "r+")
    lines <- readLines(fconn)
    header <- sub("\n$", "", header)
    writeLines(c(header, lines), con = fconn)
    close(fconn)
} 

# write GMT files for all combinations of ID type and taxonomic level
tax.levels <- c("mixed", "genus", "species")
id.types <- c("ncbi", "metaphlan", "taxname")
exact.tax.levels <- c(TRUE, FALSE)

for(tl in tax.levels)
{
    for(it in id.types)
    {
        for(etl in exact.tax.levels)
        {
            if(tl == "mixed") next
            sigs <- bugsigdbr::getSignatures(bsdb, 
                                             tax.id.type = it,
                                             tax.level = tl,
                                             exact.tax.level = etl)
            gmt.file <- paste("bugsigdb", "signatures", tl, it, sep = "_")
            if(etl) gmt.file <- paste(gmt.file, "exact", sep = "_") 
            gmt.file <- paste(gmt.file, "gmt", sep = ".")
            gmt.file <- file.path(out.dir, gmt.file)
            bugsigdbr::writeGMT(sigs, gmt.file = gmt.file) 
            addHeader(header, gmt.file)
        }
    }
}        

