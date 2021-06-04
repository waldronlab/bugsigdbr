bsdb <- importBugSigDB()

checkSigs <- function(sigs, tax.id.type)
{
    expect_true(is.list(sigs))
    expect_true(length(sigs) > 1500)
    expect_true(is.character(sigs[[1]]))
    expect_true(grepl("^bsdb", names(sigs)[1]))
    
    if(tax.id.type == "ncbi") 
        expect_true(grepl("^[0-9]+$", sigs[[1]][1]))
    else if(tax.id.type == "metaphlan") 
        expect_true(grepl("^[kpcofgst]__", sigs[[1]][1]))
    else if(tax.id.type == "taxname")
        expect_true(grepl("^[A-Z][a-z ]+$", sigs[[1]][1]))
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