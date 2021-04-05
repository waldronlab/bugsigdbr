test_that("importBugSigDB dimensions are greater than c(0, 0)", {
    bsdb <- importBugSigDB(cache = TRUE)
    expect_gt(nrow(bsdb), 0)
    expect_gt(ncol(bsdb), 0)
})
