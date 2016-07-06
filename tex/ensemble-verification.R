## ----echo=FALSE----------------------------------------------------------
library("SpecsVerification2", lib.loc="~/folders/jss-paper-ensemble-verification/R/libtmp")
set.seed(31415)
options(digits=4, scipen=10)
knitr::knit_theme$set("acid")
knitr::opts_knit$set(progress=FALSE)
knitr::opts_chunk$set(dev="pdf", tidy=FALSE)

## ----label=load-data-----------------------------------------------------
data(eurotempforecast)
R   <- ncol(ens)
yrs <- as.numeric(names(obs))
N   <- length(obs)

## ----label=gfs-plot, fig.width=4, fig.height=2---------------------------
par(las=1, cex=0.7, mgp=c(3, 1, 0), mar=c(2,4,1,1))
matplot(yrs, ens, ylab="temp [C]", pch=1, col=gray(.5))
points(yrs, obs, pch=15, cex=1.5)

## ------------------------------------------------------------------------
rbind(
continuous = c(obs=obs["2001"], ens["2001", 1:5]),
binary     = c(obs=obs.bin["2001"], ens.bin["2001", 1:5]),
categorical = c(obs=obs.cat["2001"], ens.cat["2001", 1:5]))

## ------------------------------------------------------------------------
i.small <- sample(1:R, 5)
i.large <- setdiff(1:R, i.small)
c(small.ens=mean(EnsBrier(ens.bin[, i.small], obs.bin)), 
  large.ens=mean(EnsBrier(ens.bin[, i.large], obs.bin)))

## ------------------------------------------------------------------------
c(small.ens=mean(EnsBrier(ens.bin[, i.small], obs.bin, R.new=19)), 
  large.ens=mean(EnsBrier(ens.bin[, i.large], obs.bin, R.new=19)))

## ------------------------------------------------------------------------
i.small <- sample(1:R, 5)
i.large <- setdiff(1:R, i.small)
cbind(
 QS = c(
  ens           = mean(EnsQs(ens.cat,            obs.cat)),
  small.ens     = mean(EnsQs(ens.cat[, i.small], obs.cat)),
  large.ens     = mean(EnsQs(ens.cat[, i.large], obs.cat)),
  small.ens.adj = mean(EnsQs(ens.cat[, i.small], obs.cat, R.new=24)),
  large.ens.adj = mean(EnsQs(ens.cat[, i.large], obs.cat, R.new=24))
 ),
 RPS = c(
  ens           = mean(EnsRps(ens.cat,            obs.cat)),
  small.ens     = mean(EnsRps(ens.cat[, i.small], obs.cat)),
  large.ens     = mean(EnsRps(ens.cat[, i.large], obs.cat)),
  small.ens.adj = mean(EnsRps(ens.cat[, i.small], obs.cat, R.new=24)),
  large.ens.adj = mean(EnsRps(ens.cat[, i.large], obs.cat, R.new=24))
 )
)

## ------------------------------------------------------------------------
cbind(
 CRPS=c(
  unadjusted = mean(EnsCrps(ens, obs)), 
  fair       = mean(EnsCrps(ens, obs, R.new=Inf))
 )
)

## ------------------------------------------------------------------------
ens.ref     <- ClimEns(obs)
ens.cat.ref <- ClimEns(obs.cat)
ens.bin.ref <- ClimEns(obs.bin)

## ------------------------------------------------------------------------
rbind(
  Brier = ScoreDiff(EnsBrier(ens.bin,     obs.bin), 
                    EnsBrier(ens.bin.ref, obs.bin)),
  QS    = ScoreDiff(EnsQs(   ens.cat,     obs.cat),    
                    EnsQs(   ens.cat.ref, obs.cat)),
  RPS   = ScoreDiff(EnsRps(  ens.cat,     obs.cat),   
                    EnsRps(  ens.cat.ref, obs.cat)),
  CRPS  = ScoreDiff(EnsCrps( ens,         obs),          
                    EnsCrps( ens.ref,     obs))
)

## ------------------------------------------------------------------------
rbind(
 Brier = SkillScore(EnsBrier(ens.bin, obs.bin), 
                    EnsBrier(ens.bin.ref, obs.bin)),
 QS    = SkillScore(EnsQs(ens.cat, obs.cat),    
                    EnsQs(ens.cat.ref, obs.cat)),
 RPS   = SkillScore(EnsRps(ens.cat, obs.cat),   
                    EnsRps(ens.cat.ref, obs.cat)),
 CRPS  = SkillScore(EnsCrps(ens, obs),          
                    EnsCrps(ens.ref, obs))
)

## ------------------------------------------------------------------------
ens.mean <- rowMeans(ens)
Corr(ens.mean, obs, conf.level=0.95)

## ------------------------------------------------------------------------
CorrDiff(fcst=ens.mean, fcst.ref=obs.lag, obs=obs, conf.level=0.95)

## ------------------------------------------------------------------------
rbind(
  large.ens = Auc(rowMeans(ens.bin[, i.large]), obs.bin),
  small.ens = Auc(rowMeans(ens.bin[, i.small]), obs.bin)
)

## ------------------------------------------------------------------------
AucDiff(rowMeans(ens.bin[, i.large]), rowMeans(ens.bin[, i.small]), obs.bin)

## ------------------------------------------------------------------------
(rh <- Rankhist(ens, obs))

## ----label=rank-hist-pp, fig.width=5, fig.height=3.5---------------------
PlotRankhist(rh, mode="prob.paper")

## ------------------------------------------------------------------------
TestRankhist(rh)

## ------------------------------------------------------------------------
p.bin <- rowMeans(ens.bin)
ReliabilityDiagram(p.bin, obs.bin, plot=FALSE, bins=3)

## ----label=reldiag, fig.width=4, fig.height=4.5--------------------------
rd <- ReliabilityDiagram(p.bin, obs.bin, plot=TRUE, bins=3, attributes=TRUE)

## ------------------------------------------------------------------------
BrierDecomp(p.bin, obs.bin, bins=3, bias.corrected=TRUE)

