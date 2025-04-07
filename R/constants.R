# ORDINARY
TAX.LEVELS <- c("domain", "kingdom", "phylum", "class", "order", 
                "family", "genus", "species", "strain")


MPA.TAX.LEVELS <- c(substring(TAX.LEVELS[-9], 1, 1), "t")  # "-9" to exclude "strain"
names(MPA.TAX.LEVELS) <- TAX.LEVELS

MPA.REGEXP <- "^[dkpcofgst]__"

# URLs
EFO.OBO.URL <- "https://www.ebi.ac.uk/efo/efo.obo"
UBERON.OBO.URL <- "http://ontologies.berkeleybop.org/uberon.obo"
