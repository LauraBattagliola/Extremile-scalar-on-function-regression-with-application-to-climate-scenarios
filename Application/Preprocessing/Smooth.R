require(KernSmooth)
require(sm)
################################################################################
################################# Smoothing ####################################
################################################################################

S <- 116


load("/Code/Application/Data/Temperature/RCP26_DRY_mean_air_temp.Rdata")
load("/Code/Application/Data/Temperature/RCP85_DRY_mean_air_temp.Rdata")
load("/Code/Application/Data/Temperature/RCP26_WS_mean_air_temp.Rdata")
load("/Code/Application/Data/Temperature/RCP85_WS_mean_air_temp.Rdata")

####### Air Temperature

RCP85.DRY.mean.air.temp.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP85.DRY.mean.air.temp[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP85.DRY.mean.air.temp[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP85.DRY.mean.air.temp.smooth[[i]] <- mat
}

#--------------------------------------------------------------------------------
RCP85.WS.mean.air.temp.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP85.WS.mean.air.temp[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP85.WS.mean.air.temp[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP85.WS.mean.air.temp.smooth[[i]] <- mat
}


#------------------------------------------------------------------------------------



RCP26.DRY.mean.air.temp.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP26.DRY.mean.air.temp[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP26.DRY.mean.air.temp[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP26.DRY.mean.air.temp.smooth[[i]] <- mat
}

#--------------------------------------------------------------------------------
RCP26.WS.mean.air.temp.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP26.WS.mean.air.temp[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP26.WS.mean.air.temp[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP26.WS.mean.air.temp.smooth[[i]] <- mat
}

#############################################################################################
load("/Code/Application/Data/Humidity/RCP26_DRY_mean_rel_hum.Rdata")
load("/Code/Application/Data/Humidity/RCP85_DRY_mean_rel_hum.Rdata")
load("/Code/Application/Data/Humidity/RCP26_WS_mean_rel_hum.Rdata")
load("/Code/Application/Data/Humidity/RCP85_WS_mean_rel_hum.Rdata")

####### Relative humidity

RCP85.DRY.mean.rel.hum.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP85.DRY.mean.rel.hum[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP85.DRY.mean.rel.hum[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP85.DRY.mean.rel.hum.smooth[[i]] <- mat
}

#--------------------------------------------------------------------------------
RCP85.WS.mean.rel.hum.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP85.WS.mean.rel.hum[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP85.WS.mean.rel.hum[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP85.WS.mean.rel.hum.smooth[[i]] <- mat
}


#------------------------------------------------------------------------------------



RCP26.DRY.mean.rel.hum.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP26.DRY.mean.rel.hum[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP26.DRY.mean.rel.hum[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP26.DRY.mean.rel.hum.smooth[[i]] <- mat
}

#--------------------------------------------------------------------------------
RCP26.WS.mean.rel.hum.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP26.WS.mean.rel.hum[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP26.WS.mean.rel.hum[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP26.WS.mean.rel.hum.smooth[[i]] <- mat
}

#------------------------------------------------------------------------
## Wind speed
S <- 116


load("/Code/Application/Data/Wind/RCP26_DRY_mean_wind_speed.Rdata")
load("/Code/Application/Data/Wind/RCP85_DRY_mean_wind_speed.Rdata")
load("/Code/Application/Data/Wind/RCP26_WS_mean_wind_speed.Rdata")
load("/Code/Application/Data/Wind/RCP85_WS_mean_wind_speed.Rdata")

####### Air Temperature

RCP85.DRY.mean.wind.speed.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP85.DRY.mean.wind.speed[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP85.DRY.mean.wind.speed[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP85.DRY.mean.wind.speed.smooth[[i]] <- mat
}

#--------------------------------------------------------------------------------
RCP85.WS.mean.wind.speed.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP85.WS.mean.wind.speed[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP85.WS.mean.wind.speed[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP85.WS.mean.wind.speed.smooth[[i]] <- mat
}


#------------------------------------------------------------------------------------



RCP26.DRY.mean.wind.speed.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP26.DRY.mean.wind.speed[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP26.DRY.mean.wind.speed[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP26.DRY.mean.wind.speed.smooth[[i]] <- mat
}

#--------------------------------------------------------------------------------
RCP26.WS.mean.wind.speed.smooth <- list()

for(i in 1:41){
  mat <- matrix(nrow=12,ncol=S)
  for(m in 1:12){
    hcv = h.select(1:24, RCP26.WS.mean.wind.speed[[i]][m,])
    mat[m,]  <- locpoly(1:24,
                        RCP26.WS.mean.wind.speed[[i]][m,],  degree = 3, kernel = "normal",
                        bandwidth = hcv,
                        gridsize = S, bwdisc = 100, range.x = c(1,24),
                        binned = FALSE, truncate = TRUE)$y
  }
  RCP26.WS.mean.wind.speed.smooth[[i]] <- mat
}

#############################################################################################3

setwd("/Code/Application/Data/Temperature")
save(RCP85.WS.mean.air.temp.smooth, file = "RCP85_WS_mean_air_temp_smooth.Rdata")
save(RCP26.WS.mean.air.temp.smooth, file = "RCP26_WS_mean_air_temp_smooth.Rdata")
save(RCP85.DRY.mean.air.temp.smooth, file = "RCP85_DRY_mean_air_temp_smooth.Rdata")
save(RCP26.DRY.mean.air.temp.smooth, file = "RCP26_DRY_mean_air_temp_smooth.Rdata")

setwd("/Code/Application/Data/Humidity")
save(RCP85.WS.mean.rel.hum.smooth, file = "RCP85_WS_mean_rel_hum_smooth.Rdata")
save(RCP26.WS.mean.rel.hum.smooth, file = "RCP26_WS_mean_rel_hum_smooth.Rdata")
save(RCP85.DRY.mean.rel.hum.smooth, file = "RCP85_DRY_mean_rel_hum_smooth.Rdata")
save(RCP26.DRY.mean.rel.hum.smooth, file = "RCP26_DRY_mean_rel_hum_smooth.Rdata")

setwd("/Code/Application/Data/Wind")
save(RCP85.WS.mean.wind.speed.smooth, file = "RCP85_WS_mean_wind_speed_smooth.Rdata")
save(RCP26.WS.mean.wind.speed.smooth, file = "RCP26_WS_mean_wind_speed_smooth.Rdata")
save(RCP85.DRY.mean.wind.speed.smooth, file = "RCP85_DRY_mean_wind_speed_smooth.Rdata")
save(RCP26.DRY.mean.wind.speed.smooth, file = "RCP26_DRY_mean_wind_speed_smooth.Rdata")


