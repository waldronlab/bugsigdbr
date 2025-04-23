.onAttach <- function(libname, pkgname) {
    msg <- paste("Note: After Feb. 16, 2025 PubMed ID replaced Study ID in",
                 "BugSigDb. See",
                 "https://github.com/waldronlab/BugSigDB/issues/263.")
    packageStartupMessage(msg)
}
