# Jake Yeung
# Date of Creation: 2018-09-13
# File: ~/projects/machine_learning_playground/scripts/functions/LmmFunctions.R
# Functions for linear mixed models


MatToLong <- function(dat.rep, gname, tnames, znames, cname = "exprs", repname = ""){
  # cname can be "exprs" or "exprs.sd"
  dat.vec <- matrix(dat.rep, ncol = 1)
  # output as dataframe
  dat.long.rep <- data.frame(gene = gname,
                             time = rep(tnames, each = nrow(dat.rep)),
                             zone = rep(znames, ncol(dat.rep)),
                             jrep = repname)
  dat.long.rep[[cname]] <- dat.vec
  return(dat.long.rep)
}

AllRepsToLong <- function(dat.gene, gname, indices, tnames, znames, cname = "exprs"){
  dat.long.lst <- lapply(indices, function(i){
    return(MatToLong(dat.gene[[i]], gname, tnames, znames, cname = cname, repname = i))
  })
  return(dplyr::bind_rows(dat.long.lst))
}

GetCosPart <- function(tvec, w = 2 * pi / 24){
  return(cos(w * tvec))
}

GetSinPart <- function(tvec, w = 2 * pi / 24){
  return(sin(w * tvec))
}

MakeDatPred <- function(dat.pred, stanmodel, nsamps, replace.list, tvec.pred, zvec.pred){
  link.stan <- link(stanmodel, n = nsamps, replace = replace.list, data = d.pred)
  pred.p.mean <- apply( link.stan, 2, mean )
  pred.p.PI <- apply( link.stan, 2, PI, prob=0.9 )
  # make long
  dat.pred.long <- data.frame(
    time = tvec.pred, 
    zone = zvec.pred,
    mouse = 1,
    exprs = pred.p.mean,
    exprs.lower = pred.p.PI[1, ],
    exprs.upper = pred.p.PI[2, ]
  )
  return(dat.pred.long)
}

AddTimeColors <- function(dat){
  dat$col <- sapply(dat$time, function(x) hsv(PhaseToHsv(x, 0, 24), s = 1, v = 1))
  return(dat)
}

PlotPredicts <- function(dat.pred, dat.real, jtitle = ""){
  dat.pred <- AddTimeColors(dat.pred)
  dat.real <- AddTimeColors(dat.real)
  
  jfilt <- dat.pred %>% filter(time %in% c(0, 6, 12, 18))
  p1 <- ggplot(dat.pred, aes(x = time, y = exprs)) + 
    geom_ribbon(aes(ymin = exprs.lower, ymax = exprs.upper), alpha = 0.1, data = dat.pred) + 
    geom_line(data = dat.pred) + facet_wrap(~zone) + 
    geom_point(data = dat.real) + 
    theme_bw() + theme(aspect.ratio=1, panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle(jtitle)
  p2 <- ggplot(jfilt, aes(x = zone, y = exprs)) + 
    geom_ribbon(aes(ymin = exprs.lower, ymax = exprs.upper), alpha = 0.1, data = jfilt) + 
    geom_line(data = jfilt) + facet_wrap(~time) + 
    geom_point(data = dat.real) + 
    theme_bw() + theme(aspect.ratio=1, panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle(jtitle)
  # p3 <- ggplot(jfilt, aes(x = zone, y = exprs, group = time, color = col)) +
  #         geom_line(data = jfilt) +
  #         geom_point(data = dat.real) + 
  #         theme_bw() + theme(aspect.ratio=1, panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle(jtitle) +
  #         scale_color_identity() + facet_wrap(~time)
  p4 <- ggplot(jfilt, aes(x = zone, y = exprs, group = time, color = col)) +
    geom_line(data = jfilt) + 
    geom_point(data = dat.real) + 
    theme_bw() + theme(aspect.ratio=1, panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle(jtitle) +
    scale_color_identity()
  return(list(p1, p2, p4))
}
