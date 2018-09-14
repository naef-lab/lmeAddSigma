### nlmer() convergence testing / monitoring / ...
##  -------------------

### The output of tests here are *not* 'diff'ed  (<==> no *.Rout.save file)
library(lmeAddSigma)

## 'Theoph' Data modeling

if (lmeAddSigma:::testLevel() > 1) {
    Th.start <- c(lKe=-2.5, lKa=0.5, lCl=-3)

    (nm2 <- nlmer(conc ~ SSfol(Dose, Time,lKe, lKa, lCl) ~ lKe+lKa+lCl|Subject,
                  Theoph, start = Th.start))
    (nm3 <- nlmer(conc ~ SSfol(Dose, Time,lKe, lKa, lCl) ~
                  (lKe|Subject)+(lKa|Subject)+(lCl|Subject),
                  Theoph, start = Th.start))
    ## dropping   lKe  from random effects:
    (nm4 <- nlmer(conc ~ SSfol(Dose, Time,lKe, lKa, lCl) ~lKa+lCl|Subject,
                  Theoph, start = Th.start, tolPwrss=1e-8))
    (nm5 <- nlmer(conc ~ SSfol(Dose, Time,lKe, lKa, lCl) ~(lKa|Subject)+(lCl|Subject),
                  Theoph, start = Th.start))
}
