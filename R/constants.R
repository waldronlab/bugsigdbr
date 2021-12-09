# ORDINARY
TAX.LEVELS <- c("kingdom", "phylum", "class", "order", "family", "genus",
                "species", "strain")
MPA.TAX.LEVELS <- c(substring(TAX.LEVELS[-8], 1, 1), "t")
names(MPA.TAX.LEVELS) <- TAX.LEVELS
MPA.REGEXP <- "^[kpcofgst]__"

# URLs
EFO.OBO.URL <- "https://www.ebi.ac.uk/efo/efo.obo"
UBERON.OBO.URL <- "http://ontologies.berkeleybop.org/uberon.obo"
