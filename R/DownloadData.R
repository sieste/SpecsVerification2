#############################################################################
# retrieve the data from ECOMS database
#############################################################################

load.ecoms <- FALSE
if (load.ecoms) {

  # target variable: 2-meter-temperature
  var <- "tas" 
  # latitude and longitude limits for SPECS region (roughly Europe) 
  lats <- c(30,75) 
  lons <- c(-12.5,42.5) 
  # target period: JJA
  season <- c(6,7,8) 
  # lead time 1 month, i.e. initialized on 1 May for the above target
  lead <- 1 
  
  library(ecomsUDG.Raccess)
  loginECOMS_UDG("sieste", "hackme")

  # NCEP CFSv2
  ens.data <-  loadECOMS(dataset="CFSv2_seasonal", 
                         var=var,
                         season=season,
                         leadMonth=lead, 
                         members=1:24, 
                         latLim=lats,
                         lonLim=lons,
                         years=1982:2010,
                         time="DD", 
                         aggr.d="mean",
                         aggr.m="mean")

  # NCEP reanalysis 
  obs.data <- loadECOMS(dataset="NCEP",
                        var=var,
                        season=season,
                        latLim=lats,
                        lonLim=lons,
                        years=1982:2010,
                        time="DD", 
                        aggr.d="mean",
                        aggr.m="mean")

  save(file="~/folders/jss-paper-ensemble-verification/data/ncep-seas-eu-t2m-raw.Rdata",
       list=c("ens.data", "obs.data"))
}
load("~/folders/jss-paper-ensemble-verification/data/ncep-seas-eu-t2m-raw.Rdata")



#############################################################################
# calculate space-time averages
#############################################################################

# name dimensions
ens <- ens.data[['Data']]
dimnames(ens) <- list(
  member=ens.data[['Members']],
  t.ver=format(as.Date(ens.data$Dates$start), "%Y-%m"),
  lat=paste(ens.data[['xyCoords']][['y']]),
  lon=paste(ens.data[['xyCoords']][['x']]))
obs <- obs.data[['Data']]
dimnames(obs) <- list(
  t.ver=format(as.Date(obs.data$Dates$start), "%Y-%m"),
  lat=paste(obs.data[['xyCoords']][['y']]),
  lon=paste(obs.data[['xyCoords']][['x']]))


# calculate longitudinal averages
ens <- apply(X=ens, MARGIN=c("member", "t.ver", "lat"), mean)
obs <- apply(X=obs, MARGIN=c("t.ver", "lat"), mean)

# calculate area-weighted latitudinal means
coslat.ens <- cos(as.numeric(dimnames(ens)[['lat']]) / 360. * 2. * pi)
coslat.obs <- cos(as.numeric(dimnames(obs)[['lat']]) / 360. * 2. * pi)
ens <- apply(X=ens, MARGIN=c("member", "t.ver"), weighted.mean, w=coslat.ens)
obs <- apply(X=obs, MARGIN=c("t.ver"), weighted.mean, w=coslat.obs)

# calculate lagged observation
ens.year1 <- format(as.Date(paste(dimnames(ens)[['t.ver']][1], "-01", sep="")), "%Y")
nlag1 <- grep(as.numeric(ens.year1)-1, names(obs))
stopifnot(length(nlag1) > 0)
obs.prev <- mean(obs[nlag1])

# homogenize times between forecasts and observations
obs <- obs[ dimnames(ens)[['t.ver']] ]

# transpose, have ensemble members as columns and dates as rows
ens <- t(ens)

# aggregate months
times <- as.Date(paste(names(obs), "-01", sep=""))
years <- format(times, "%Y")
uniq.year <- unique(years)
ens.tmp <- array(NA_real_, 
                 dim=c(length(uniq.year), ncol(ens)),
                 dimnames=list(year=uniq.year, member=colnames(ens)))
obs.tmp <- rep(NA_real_, length(uniq.year))
names(obs.tmp) <- uniq.year
for (y in uniq.year) {
  inds <- which(years == y)
  obs.tmp[y] <- mean(obs[inds])
  ens.tmp[y, ] <- colMeans(ens[inds, ])
}
ens <- ens.tmp
obs <- obs.tmp


# lagged observation for use in data set
obs.lag <- c(obs.prev, obs[1:(length(obs)-1)])
names(obs.lag) <- names(obs)


# save
save(file="~/folders/jss-paper-ensemble-verification/data/ncep-seas-eu-t2m.Rdata",
     list=c("ens", "obs", "obs.lag"))


# debiased ensemble data for use in R data set
ens <- ens - mean(ens) + mean(obs)


# make binary and categorical forecasts
N <- length(obs)
obs.bin <- 1 * (obs > obs.lag)
ens.bin <- 1 * (ens > obs.lag)
names(obs.bin) <- names(obs)
dimnames(ens.bin) <- dimnames(ens)


categ <- function(x, cat.ctr, half.width) {
  as.numeric(cut(x, breaks=c(-Inf, cat.ctr-half.width, cat.ctr+half.width, Inf)))
}

obs.cat <- sapply(1:N, function(i) categ(obs[i], obs.lag[i], 0.25))
ens.cat <- sapply(1:ncol(ens), function(j) {
                  sapply(1:N, function(i) categ(ens[i, j], obs.lag[i], 0.25))
           })
names(obs.cat) <- names(obs)
dimnames(ens.cat) <- dimnames(ens)




save(file="~/folders/jss-paper-ensemble-verification/R/SpecsVerification2/data/eurotempforecast.rda",
     list=c("ens", "obs", "obs.lag", "obs.bin", "ens.bin", "obs.cat", "ens.cat"))

