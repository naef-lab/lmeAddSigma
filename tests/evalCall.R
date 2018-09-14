## see if we can still run lmeAddSigma functions when lmeAddSigma is not attached
if ("package:lmeAddSigma" %in% search()) detach("package:lmeAddSigma")
data(sleepstudy,package="lmeAddSigma")
data(cbpp,package="lmeAddSigma")
fm1 <- lmeAddSigma::lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
gm1 <- lmeAddSigma::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             data = cbpp, family = binomial)
