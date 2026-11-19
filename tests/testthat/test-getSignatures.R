bsdb <- importBugSigDB()

checkSigs <- function(sigs, tax.id.type)
{
    expect_true(is.list(sigs))
    expect_true(is.character(sigs[[1]]))
    expect_true(grepl("^bsdb", names(sigs)[1]))
    
    if(tax.id.type == "ncbi") 
        expect_true(grepl("^[0-9]+$", sigs[[1]][1]))
    else if(tax.id.type == "metaphlan") 
        expect_true(grepl("^[dkpcofgst]__", sigs[[1]][1]))
    else if(tax.id.type == "taxname")
        expect_true(grepl("^[A-Za-z ]+$", sigs[[1]][1]))
}

test_that("tax.id.type", {
    sigs <- getSignatures(bsdb)
    checkSigs(sigs, tax.id.type = "ncbi")
    
    sigs <- getSignatures(bsdb, tax.id.type = "metaphlan")
    checkSigs(sigs, tax.id.type = "metaphlan")
    
    sigs <- getSignatures(bsdb, tax.id.type = "taxname")
    checkSigs(sigs, tax.id.type = "taxname")
})

test_that("tax.level", {
    sigs <- getSignatures(bsdb, tax.id.type = "metaphlan", tax.level = "genus")
    checkSigs(sigs, tax.id.type = "metaphlan")
    sigs <- getSignatures(bsdb, tax.id.type = "metaphlan", tax.level = "species")
    checkSigs(sigs, tax.id.type = "metaphlan")
    sigs <- getSignatures(bsdb, tax.id.type = "ncbi", tax.level = "genus")
    checkSigs(sigs, tax.id.type = "ncbi")
    sigs <- getSignatures(bsdb, tax.id.type = "taxname", tax.level = "species")
    checkSigs(sigs, tax.id.type = "taxname")
})    

test_that("exact.tax.level", {
  sigs <- getSignatures(bsdb, tax.id.type = "metaphlan", 
                        tax.level = "family", exact.tax.level = FALSE)
  checkSigs(sigs, tax.id.type = "metaphlan")
  sigs <- getSignatures(bsdb, tax.id.type = "metaphlan", 
                        tax.level = "order", exact.tax.level = FALSE)
  checkSigs(sigs, tax.id.type = "metaphlan")
})  

test_that("min.size", {
    sigs <- getSignatures(bsdb, tax.level = "species")
    expect_true(all(lengths(sigs) > 0))
    
    sigs <- getSignatures(bsdb, tax.level = "species", min.size = 0)
    expect_false(all(lengths(sigs) > 0))
    expect_true(all(lengths(sigs) >= 0))

    sigs <- getSignatures(bsdb, tax.level = "genus", min.size = 3)
    expect_true(all(lengths(sigs) > 2))
    expect_false(all(lengths(sigs) > 3))
    expect_false(any(lengths(sigs) < 3))
})    



test_that("experiments without signatures are skipped", {
    # releases from v1.4.0 include experiments without signatures, with NA
    # signature columns
    no.sig <- is.na(bsdb[["Signature page name"]])
    expect_true(any(no.sig))
    sigs <- getSignatures(bsdb, tax.id.type = "ncbi", min.size = 0)
    expect_length(sigs, sum(!no.sig))
    expect_false(any(grepl("^bsdb:[^_]*/NA_", names(sigs))))
})
