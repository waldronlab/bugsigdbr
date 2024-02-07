bsdb <- importBugSigDB()
df.feces <- subset(bsdb, `Body site` == "Feces")
bs.msigs <- getMetaSignatures(bsdb, "Body site")
co.msigs <- getMetaSignatures(df.feces, "Condition")

checkSigs <- function(sigs, tax.id.type)
{
    expect_true(is.list(sigs))
    ind <- sample(length(sigs), 1)
    s <- sigs[[ind]]
    expect_true(is.numeric(s))   
    expect_true(all(s >= 0))   
    expect_true(all(s <= 1))   
    expect_false(is.null(names(s)))
 
    if(tax.id.type == "ncbi") 
        expect_true(all(grepl("^[0-9]+$", names(s))))
    else if(tax.id.type == "metaphlan") 
        expect_true(all(grepl("^[kpcofgst]__", names(s))))
    else if(tax.id.type == "taxname")
        expect_true(all(grepl("^[A-Za-z ]+$", names(s))))
}

test_that("column of choice", {
    checkSigs(bs.msigs, tax.id.type = "ncbi")
    checkSigs(co.msigs, tax.id.type = "ncbi")
})

test_that("direction", {
    sigs <- getMetaSignatures(bsdb, "Body site", direction = "DOWN")    
    checkSigs(sigs, tax.id.type = "ncbi")
    expect_false(sigs[[1]][1] == bs.msigs[[1]][1])
})    

test_that("min.studies", {
    spl <- split(bsdb$PMID, bsdb[["Body site"]])
    ind <- lengths(spl) < 2
    expect_false(any(names(spl)[ind] %in% names(bs.msigs)))
    
    bs.msigs.3 <- getMetaSignatures(bsdb, "Body site", min.studies = 3)
    ind <- lengths(spl) < 3
    expect_false(any(names(spl)[ind] %in% names(bs.msigs.3)))

    spl <- split(df.feces$PMID, df.feces[["Condition"]])
    ind <- lengths(spl) < 2
    expect_false(any(names(spl)[ind] %in% names(co.msigs)))
})  

test_that("min.taxa", {
    expect_true(all(lengths(bs.msigs) > 4))    
    expect_true(all(lengths(co.msigs) > 4))
    bs.msigs.8 <- getMetaSignatures(bsdb, "Body site", min.taxa = 8)
    checkSigs(bs.msigs.8, tax.id.type = "ncbi")
    expect_true(all(lengths(bs.msigs.8) > 7)) 
})    

test_that("comb.fun", {
    sigs <- getMetaSignatures(bsdb, "Body site", comb.fun = min)
    checkSigs(sigs, tax.id.type = "ncbi")
    expect_true(all(lengths(sigs) == lengths(bs.msigs)))
    expect_false(sigs[[1]][1] == bs.msigs[[1]][1])
})    

test_that("...", {
    sigs <- getMetaSignatures(bsdb, "Body site", tax.id.type = "metaphlan")
    checkSigs(sigs, tax.id.type = "metaphlan")
    
    sigs <- getMetaSignatures(bsdb, "Body site",
                              tax.level = "genus",
                              exact.tax.level = FALSE,   
                              tax.id.type = "taxname")
    checkSigs(sigs, tax.id.type = "taxname")
})
