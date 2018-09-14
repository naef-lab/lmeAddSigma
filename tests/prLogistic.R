## data set and formula extracted from ?prLogisticDelta example
##   (Thailand, clustered-data) in prLogistic package
load(system.file("testdata","prLogistic.RData",package="lmeAddSigma"))
library(lmeAddSigma)

(testLevel <- lmeAddSigma:::testLevel())
if (testLevel > 2) {
    print(system.time(
    lmeAddSigma_est <- glmer(rgi ~ sex + pped + (1|schoolid),
                      data = dataset, family=binomial)
    ))
    lmeAddSigma_results <- list(sigma= sqrt(unname(unlist(VarCorr(lmeAddSigma_est)))),
                         beta = fixef(lmeAddSigma_est))

    ## stored results from other pkgs
    glmmML_est <- list(sigma = 1.25365353546143,
                       beta = c("(Intercept)" = -2.19478801858317,
                           "sex" = 0.548884468743364, "pped"= -0.623835613907385))
    lmeAddSigma.0_est <- list(sigma = 1.25369539060849,
                       beta = c("(Intercept)" = -2.19474529099587,
                           "sex" = 0.548900267825802, "pped"= -0.623934772981894))

    source(system.file("test-tools-1.R", package = "Matrix"))#-> assert.EQ() etc
    assert.EQ.(lmeAddSigma_results, glmmML_est, tol=3e-3)
    assert.EQ.(lmeAddSigma_results, lmeAddSigma.0_est, tol=3e-3)
    print(lmeAddSigma_est)
}
