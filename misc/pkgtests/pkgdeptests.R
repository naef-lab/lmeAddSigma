## need this because R can't handle '@CRAN@' magic default
## in non-interactive mode ...
options(repos=c(CRAN="http://probability.ca/cran",
        rforge="http://r-forge.r-project.org",
        bioc="http://www.bioconductor.org/packages/release/bioc"))

source("pkgdepfuns.R")    ## define functions
rr <- getDepends("lmeAddSigma")  ## download dependency structure from CRAN
pkgnotes <- read.csv("lmeAddSigma_notes.csv")  ## 
testresults <- doPkgDeptests("lmeAddSigma",verbose=TRUE,do_parallel=FALSE)
save("testresults",file="lmeAddSigmatests_out.RData")
genReport(rr,testresults,extra.info=pkgnotes)

###

if (FALSE) {
    ## playing with results
    L <- load("lmeAddSigmatests_out.RData")
    rr <- getDepends("lmeAddSigma")
    xx <- read.csv("lmeAddSigma_notes.csv")
    genReport(rr,testresults,extra=xx)
    checkPkg("HSAUR2")
    checkPkg("car",checkdir="check")
    checkPkg("difR",checkdir="check",verbose=TRUE)
}



