.onLoad <- function(libname, pkgname) {
    options(lmeAddSigma.summary.cor.max = 12)
}

.onUnload <- function(libpath) {
    gc()
    if (is.loaded("lmer_Deviance", PACKAGE="lmeAddSigma"))
        library.dynam.unload("lmeAddSigma", libpath)
}
