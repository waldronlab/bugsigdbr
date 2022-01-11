test_that("importBugSigDB dimensions are greater than c(0, 0)", {
    bsdb <- importBugSigDB(cache = FALSE, version = "devel")
    expect_gt(nrow(bsdb), 0)
    expect_gt(ncol(bsdb), 0)
})

test_that("importBugSigDB comes from Zenodo", {
    bsdb <- bugsigdbr::importBugSigDB(cache = FALSE,
                                      version = "10.5281/zenodo.5819260")
    url <- "https://zenodo.org/record/5819260/files/full_dump.csv?download=1"
    dat <- suppressWarnings(vroom::vroom(url, skip = 1L, progress = FALSE,
                                         show_col_types = FALSE))
    dat <- as.data.frame(dat)
    dat[["MetaPhlAn taxon names"]] <- strsplit(dat[["MetaPhlAn taxon names"]],
                                               ",")
    dat[["NCBI Taxonomy IDs"]] <- strsplit(dat[["NCBI Taxonomy IDs"]], ";")
    expect_true(identical(bsdb, dat))
})

test_that("importBugSigDB devel comes from the edge", {
    bsdb <- bugsigdbr::importBugSigDB(cache = FALSE, version = "devel")
    url <- "https://tinyurl.com/3nvzm3fx"
    dat <- suppressWarnings(vroom::vroom(url, skip = 1L, progress = FALSE,
                                         show_col_types = FALSE))
    dat <- as.data.frame(dat)
    dat[["MetaPhlAn taxon names"]] <- strsplit(dat[["MetaPhlAn taxon names"]],
                                               ",")
    dat[["NCBI Taxonomy IDs"]] <- strsplit(dat[["NCBI Taxonomy IDs"]], ";")
    expect_true(identical(bsdb, dat))
})

test_that("importBugSigDB devel comes from github", {
    bsdb <- bugsigdbr::importBugSigDB(cache = FALSE, version = "30383a9")
    url <- paste0("https://raw.githubusercontent.com/waldronlab/BugSigDBExports/",
                  "30383a9/full_dump.csv")
    dat <- suppressWarnings(vroom::vroom(url, skip = 1L, progress = FALSE,
                                         show_col_types = FALSE))
    dat <- as.data.frame(dat)
    dat[["MetaPhlAn taxon names"]] <- strsplit(dat[["MetaPhlAn taxon names"]],
                                               ",")
    dat[["NCBI Taxonomy IDs"]] <- strsplit(dat[["NCBI Taxonomy IDs"]], ";")
    expect_true(identical(bsdb, dat))
})

test_that("importBugSigDB reports error message with bad version", {
  expect_error(bugsigdbr::importBugSigDB(cache = FALSE, version = "abcdef1234"),
               paste("Version abcdef1234 does not exist. The version must be a DOI,",
                     "such as '10.5281/zenodo.5819260', a Github hash, such as",
                     "'30383a9', or 'devel'.", sep = " "))
})
