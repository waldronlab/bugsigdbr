ord <- "k__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales" 
sig <- c("f__Lactobacillaceae|g__Lactobacillus",
         "f__Aerococcaceae|g__Abiotrophia|s__Abiotrophia defectiva",
         "f__Lactobacillaceae|g__Limosilactobacillus|s__Limosilactobacillus mucosae") 
sig <- paste(ord, sig, sep = "|")

checkSig <- function(sig, tax.id.type)
{
    expect_true(is.character(sig))
    if(tax.id.type == "metaphlan") 
        expect_true(grepl("^[kpcofgst]__", sig[1]))
    else if(tax.id.type == "taxname")
        expect_true(grepl("^[A-Z][a-z ]+$", sig[1]))
}

test_that("tax.id.type", {
    s <- extractTaxLevel(sig)
    checkSig(s, tax.id.type = "metaphlan")
    expect_true(all(s == sig))
    
    s <- extractTaxLevel(sig, tax.id.type = "taxname")
    checkSig(s, tax.id.type = "taxname")
    expect_true(length(s) == length(sig))
})

test_that("tax.level", {
    s <- extractTaxLevel(sig, tax.level = "genus")
    checkSig(s, tax.id.type = "metaphlan")
    expect_true(length(s) == 1)   

    s <- extractTaxLevel(sig, tax.id.type = "taxname", tax.level = "species")
    checkSig(s, tax.id.type = "taxname")
    expect_true(length(s) == 2)

    s <- extractTaxLevel(sig, tax.id.type = "metaphlan", tax.level = "species")
    checkSig(s, tax.id.type = "metaphlan")
    expect_true(length(s) == 2)
})    

test_that("exact.tax.level", {
    s <- extractTaxLevel(sig, tax.id.type = "metaphlan", 
                        tax.level = "family", exact.tax.level = FALSE)
    checkSig(s, tax.id.type = "metaphlan")    
    tips <- bugsigdbr:::.getTip(s)
    expect_true(all(grepl("^f__", tips)))    
    expect_true(length(s) == 3)

    s <- extractTaxLevel(sig, tax.id.type = "metaphlan", 
                         tax.level = "order", exact.tax.level = FALSE)
    checkSig(sig, tax.id.type = "metaphlan")
    tips <- bugsigdbr:::.getTip(s)
    expect_true(all(grepl("^o__", tips)))
    expect_true(length(s) == 3)
})  

