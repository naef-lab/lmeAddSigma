# Jake Yeung
# Date of Creation: 2018-09-13
# File: ~/projects/machine_learning_playground/scripts/hack_lmeAddSigma_with_sigma.R
# Hack lmeAddSigma use custom sigma
# lmeAddSigma implementation notes
# http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.205.1072&rep=rep1&type=pdf

rm(list=ls())

lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

library(devtools)
library(dplyr)
library(R.matlab)
# dev_mode(T)  # does not interfere with other installed packages
# install_local("/home/yeung/projects/lmeAddSigmasigma")
# install_local("/home/yeung/projects/lmeAddSigmapureR")

library(lmeAddSigma)
# library(lmeAddSigmapureR)


# Functions ---------------------------------------------------------------

w <- 2 * pi / 24
source("/home/yeung/projects/machine_learning_playground/scripts/functions/LmmFunctions.R")
# Load data ---------------------------------------------------------------

dat <- readMat("/data/shared/ZonationAnalysis/Data/data.mat")

mean.indx <- c(1, 3, 5)
std.indx <- c(2, 4, 6)
ntimes <- 4
nzones <- 8
tnames <- paste("T", seq(ntimes), sep = "")
tnames <- seq(from = 0, by = 6, length.out = ntimes)
# znames <- paste("Z", seq(nzones), sep = "")  # PC(1) PP is 8
znames <- seq(from = 0, length.out = nzones)  # PC(1) PP is 8

# do all genes
jgenes <- names(dat)

dat.long <- lapply(jgenes, function(jgene){
  dat.gene <- AllRepsToLong(dat[[jgene]], jgene, mean.indx, tnames, znames, cname = "exprs")
})
dat.long <- dplyr::bind_rows(dat.long)
dat.long <- dat.long[!is.nan(dat.long$exprs), ]
dat.long$mouse <- paste("M", interaction(as.character(dat.long$time), as.character(dat.long$jrep)), sep = "_")
dat.long$zonenorm <- (dat.long$zone) / nzones
dat.long$cospart <- GetCosPart(dat.long$time)
dat.long$sinpart <- GetSinPart(dat.long$time)

dat.gene <- subset(dat.long, gene == "pck1")
dat.gene$exprs_scaled <- scale(dat.gene$exprs, center = TRUE, scale = TRUE)
dat.gene$timechar <- paste("T", dat.gene$time, sep = "_")

# mouse-specific intercept only... does ZxR capture the noise from zone effect?
lme.ZxR <- lmeAddSigmasigma::lmer(exprs ~ 1 + (1 | mouse) + cospart + sinpart + zone + I(zone ^ 2) + zone : timechar, data = dat.gene, REML = FALSE)
# lme.ZxR.orig <- lmeAddSigma::lmer(exprs ~ 1 + (1 | mouse) + cospart + sinpart + zone + I(zone ^ 2) + zone : timechar, data = dat.gene, REML = FALSE)
