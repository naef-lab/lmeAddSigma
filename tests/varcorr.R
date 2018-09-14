library(lmeAddSigma)
data(Orthodont, package="nlme")
fm1 <- lmer(distance ~ age + (age|Subject), data = Orthodont)
VarCorr(fm1)

fm2ML <- lmer(diameter ~ 1 + (1|plate) + (1|sample), Penicillin, REML=0)
VarCorr(fm2ML)

gm1 <- glmer(cbind(incidence,size-incidence) ~ period + (1|herd),data=cbpp,
             family=binomial)
VarCorr(gm1)

cbpp$obs <- factor(seq(nrow(cbpp)))
gm2 <- update(gm1,.~.+(1|obs))
VarCorr(gm2)

if (FALSE) {
  ## testing lmeAddSigma/lmeAddSigma incompatibility
##  library(lmeAddSigma)
  VarCorr(fm1)
  lmeAddSigma:::VarCorr.merMod(fm1) ## OK
}
