library(ontologyIndex)

bsdb <- importBugSigDB()
efo <- getOntology("efo")
uberon <- getOntology("uberon")

checkOnto <- function(onto, which = c("efo", "uberon"))
{
    which <- match.arg(which)

    expect_true(is(onto, "ontology_index"))
    expect_true(length(onto) >= 6)
    n <- c("id", "name", "parents", "children", "ancestors")
    expect_true(all(n %in% names(onto)))
    
    ostr <- attr(onto, "version")
    ostr <- grep("^ontology: ", ostr, value = TRUE)
    expect_true(grepl(which, ostr))
}

checkSubset <- function(sdf, col, pos, neg)
{
    expect_true(is.data.frame(sdf))
    expect_true(nrow(sdf) > 0)
    rel.cols <- c("Body site", "Condition")
    expect_true(all(rel.cols %in% colnames(sdf)))
    expect_true(all(pos %in% sdf[,col]))
    expect_false(any(neg %in% sdf[,col]))
} 

test_that("getOntology", {
    checkOnto(efo, "efo")
    checkOnto(uberon, "uberon")
})

test_that("subsetByOntology", { 

    bterms <- c("excreta", "digestive system", "reproductive system")
    bpos <- list( c("Feces", "Meconium", "Urine"),
                  c("Stomach", "Colon", "Mouth"),
                  c("Vagina", "Uterus", "Uterine cervix"))
    bneg <- list( c("Stomach", "Colon", "Mouth"),
                  c("Feces", "Meconium", "Urine"),
                  c("Feces", "Meconium", "Urine"))
    names(bpos) <- names(bneg) <- bterms

    cterms <- c("cancer", "nervous system disease", "metabolic disease")
    cpos <- list( c("Gastric cancer", "Cervical cancer", "Breast cancer"),
                  c("Autism", "Unipolar depression", "Schizophrenia"),
                  c("Obesity", "Type II diabetes mellitus", "Gestational diabetes"))
    cneg <- list( c("Obesity", "Anorexia nervosa", "Gestational diabetes"),
                  c("Gastric cancer", "Cervical cancer", "Breast cancer"),
                  c("Gastric cancer", "Cervical cancer", "Breast cancer"))
    names(cpos) <- names(cneg) <- cterms
    
    for(b in bterms)
    {
        sdf <- subsetByOntology(bsdb, 
                                column = "Body site",
                                term = b,
                                ontology = uberon)
        checkSubset(sdf, "Body site", bpos[[b]], bneg[[b]])
    } 

    for(ct in cterms)
    {
        sdf <- subsetByOntology(bsdb, 
                                column = "Condition",
                                term = ct,
                                ontology = efo)
        checkSubset(sdf, "Condition", cpos[[ct]], cneg[[ct]])
    }
})
