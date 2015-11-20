#############################################################################
#
# the big data files downloaded in the first block are actually stored in
# ~/folders/debias-ignorance/
#
#############################################################################

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

  # system4_seasonal_51, all 51 members, all years, daily averages
  system4.data <- loadECOMS(dataset= "System4_seasonal_51", var=var, members=1:51, 
                            lonLim=lons, latLim=lats, season=season, 
                            leadMonth=lead, time="DD")
  save(file="~/folders/debias-ignorance/system4.Rdata", list="system4.data")

  # WATCH gridded observation, all years, daily averages
  obs.data <- loadECOMS(dataset="WFDEI", var=var, lonLim=lons, latLim=lats, 
                        season=season, time="DD")
  save(file="~/folders/debias-ignorance/obs.Rdata", list="obs.data")
}



#############################################################################
# load saved data (these are HUGE files)
#############################################################################
load.data <- FALSE
if (load.data) {
  load("~/folders/debias-ignorance/obs.Rdata")
  load("~/folders/debias-ignorance/system4.Rdata")
}


#############################################################################
# regrid the observation data to the system4 model grid
#############################################################################
regrid.obs <- FALSE
if (regrid.obs) {

  # regrid the WATCH gridded observations from their native 0.5deg grid to the
  # system4 0.75deg grid; this is necessary to construct the land-sea-mask on the
  # system4 grid, to later remove the sea-points from the system4 grid; because
  # there are only observations on land available
  
  # the fields library provides the interpolation function
  library(fields)

  # get obs and model grids
  x.obs <- obs.data[["xyCoords"]][["x"]]
  y.obs <- obs.data[["xyCoords"]][["y"]]
  x.mod <- system4.data[["xyCoords"]][["x"]]
  y.mod <- system4.data[["xyCoords"]][["y"]]

  # loop over all dates
  n.dates <- dim(obs.data[["Data"]])[3]
  regridded.obs <- array(NA_real_, dim=c(length(x.mod), length(y.mod), n.dates))
  for (i in 1:n.dates) {
    # at each date, interpolate the observation onto the model grid
    regridded.obs[,,i] <- 
         interp.surface.grid(obj = list(x = x.obs, 
                             y = y.obs, z = obs.data[["Data"]][,,i]), 
                             grid.list = list(x = x.mod, y = y.mod))[["z"]]
  }
  # replace NaN's by NA's
  regridded.obs[ !is.finite(regridded.obs) ] <- NA
  # copy attributes from ECOMS dataset to regridded observations
  attributes(regridded.obs) <- list("dimensions"=c("lon","lat","time"), "dim"=c(length(x.mod), length(y.mod), n.dates))
  # replace original ECOMS data by regridded data
  obs.data[["Data"]] <- regridded.obs
  obs.data[["xyCoords"]] <- system4.data[["xyCoords"]]
  # save to disk
  save(file="data/obs.Rdata", list="obs.data")
}


#############################################################################
# calculate space-time averages
#############################################################################

calc.st.avg <- FALSE
if (calc.st.avg) {
  
  # first, calculate land sea mask from the regridded observations: NA = sea, 1
  # = land; I have checked that the NA indices in the observations are the same
  # for each time slice, so it suffices to calculate the land sea mask from the
  # first time slice; the dimensions of lsm are (lon, lat)
  lsm <- obs.data[["Data"]][,,1] * 0 + 1
  
  # calculate weight matrix for averaging: sea = 0, land = cos(lat)
  coslat <- cos(obs.data[["xyCoords"]][["y"]] * 2 * pi / 360)
  # transpose lsm -> (lat, lon); multiply each column of lsm by cos(lat);
  # transpose back to original shape
  wei.mat <- t(t(lsm) * coslat)
  # replace all NA's by zero
  wei.mat[ is.na(wei.mat) ] <- 0
  
  
  # aggregate system4 data:  
  
  # initialize
  yrs <- format.Date(system4.data[["InitializationDates"]], "%Y")
  n <- length(yrs)
  m <- length(system4.data[["Members"]])
  s4.ens <- matrix(NA_real_, nrow=n, ncol=m)
  
  # for each year, and for each ensemble member, calculate space-time mean
  for (i in 1:n) {
    i.rd <- grep(paste("^",yrs[i],sep=""), 
                 system4.data[["Dates"]][["start"]])
    for (j in 1:m) {
      avg <- 0
      for (k in i.rd) {
        xy.field <- system4.data[["Data"]][,,k,j]
        avg <- avg + sum(xy.field * wei.mat) / sum(wei.mat)
      }
      s4.ens[i,j] <- avg / length(i.rd)
    }
  }
  
  
  # aggregate obs data
  
  # initialize 
  yrs <- format.Date(system4.data[["InitializationDates"]], "%Y")
  n <- length(yrs)
  
  # for each year, for each ensemble member, calculate space-time mean
  obs <- matrix(NA_real_, nrow=n, ncol=1)
  for (i in 1:n) {
    i.rd <- grep(paste("^",yrs[i],sep=""), 
                 obs.data[["Dates"]][["start"]])
    avg <- 0
    for (k in i.rd) {
      xy.field <- obs.data[["Data"]][,,k]
      avg <- avg + sum(xy.field * wei.mat, na.rm=TRUE) / sum(wei.mat)
    }
    obs[i,1] <- avg / length(i.rd)
  }
  
  save(file="data/s4-aggregate.Rdata", list=c("obs", "s4.ens"))

}


