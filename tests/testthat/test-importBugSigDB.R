checkImport <- function(bsdb, url)
{
    expect_true(is.data.frame(bsdb))
    expect_gt(nrow(bsdb), 2000)
    expect_gt(ncol(bsdb), 40)
    expect_true(all(c("Study", "Study design", "PMID") %in% colnames(bsdb)[1:4]))

    dat <- suppressWarnings(vroom::vroom(url, skip = 1L, progress = FALSE,
                                         show_col_types = FALSE))
    dat <- as.data.frame(dat)
    dat[["MetaPhlAn taxon names"]] <- strsplit(dat[["MetaPhlAn taxon names"]],
                                               ",")
    dat[["NCBI Taxonomy IDs"]] <- strsplit(dat[["NCBI Taxonomy IDs"]], ";")
    expect_true(identical(bsdb, dat))
}

test_that("importBugSigDB from Zenodo", {
    bsdb <- bugsigdbr::importBugSigDB(version = "10.5281/zenodo.5819260", cache = FALSE)
    url <- "https://zenodo.org/record/5819260/files/full_dump.csv"
    checkImport(bsdb, url)
})

test_that("importBugSigDB from the edge (devel)", {
    bsdb <- bugsigdbr::importBugSigDB(version = "devel", cache = FALSE)
    url <- "https://tinyurl.com/3nvzm3fx"
    checkImport(bsdb, url)
})

test_that("importBugSigDB from github hash", {
    bsdb <- bugsigdbr::importBugSigDB(version = "30383a9", cache = FALSE)
    url <- paste0("https://raw.githubusercontent.com/waldronlab/BugSigDBExports/",
                  "30383a9/full_dump.csv")
    checkImport(bsdb, url)    
})

test_that("importBugSigDB error with bad version", {
  expect_error(bugsigdbr::importBugSigDB(version = "abcdef1234", cache = FALSE),
               paste("Version abcdef1234 does not exist. The version must be a DOI,",
                     "such as '10.5281/zenodo.5819260', a Github hash, such as",
                     "'30383a9', or 'devel'.", sep = " "))
})
