bsdb <- importBugSigDB()

checkDF <- function(df)
{
    expect_true(is.data.frame(df))
    expect_true(is.list(df[["NCBI Taxonomy IDs"]]))
    expect_true(is.list(df[["MetaPhlAn taxon names"]]))
    expect_true(grepl("^[0-9]+$", df[["NCBI Taxonomy IDs"]][[1]][1]))
    expect_true(grepl("^[kpcofgst]__", df[["MetaPhlAn taxon names"]][[1]][1]))
}

test_that("tax.level", {
    df <- restrictTaxLevel(bsdb)
    checkDF(df)
    
    df <- restrictTaxLevel(bsdb, tax.level = "genus")
    checkDF(df)
    tips <- bugsigdbr:::.getTip(df[["MetaPhlAn taxon names"]][[1]])
    expect_true(all(grepl("^g__", tips)))    

    df <- restrictTaxLevel(bsdb, tax.level = "family")
    checkDF(df)
    tips <- bugsigdbr:::.getTip(df[["MetaPhlAn taxon names"]][[1]])
    expect_true(all(grepl("^f__", tips)))    
})

test_that("exact.tax.level", {
    df <- restrictTaxLevel(bsdb, exact.tax.level = FALSE)
    checkDF(df)
    
    df <- restrictTaxLevel(bsdb, tax.level = "genus", exact.tax.level = FALSE)
    checkDF(df)
    tips <- bugsigdbr:::.getTip(df[["MetaPhlAn taxon names"]][[1]])
    expect_true(all(grepl("^g__", tips)))    

    df <- restrictTaxLevel(bsdb, tax.level = "family", exact.tax.level = FALSE)
    checkDF(df)
    tips <- bugsigdbr:::.getTip(df[["MetaPhlAn taxon names"]][[1]])
    expect_true(all(grepl("^f__", tips)))    
})  

test_that("min.size", {
    df <- restrictTaxLevel(bsdb, tax.level = "species")
    checkDF(df)
    expect_true(all(lengths(df[["MetaPhlAn taxon names"]]) > 0))
    
    df <- restrictTaxLevel(bsdb, tax.level = "species", min.size = 0)
    checkDF(df)
    expect_false(all(lengths(df[["MetaPhlAn taxon names"]]) > 0))
    expect_true(all(lengths(df[["MetaPhlAn taxon names"]]) >= 0))

    df <- restrictTaxLevel(bsdb, tax.level = "genus", min.size = 3)
    checkDF(df)
    expect_true(all(lengths(df[["MetaPhlAn taxon names"]]) > 2))
    expect_false(all(lengths(df[["MetaPhlAn taxon names"]]) > 3))
    expect_true(all(lengths(df[["NCBI Taxonomy IDs"]]) > 2))
    expect_false(all(lengths(df[["NCBI Taxonomy IDs"]]) > 3))
})    


