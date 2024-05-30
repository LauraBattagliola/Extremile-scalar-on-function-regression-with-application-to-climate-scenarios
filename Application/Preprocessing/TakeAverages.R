library(ggplot2)
library(dplyr)
load("Code/Application/Data/RawData/RCP26_DRY.Rdata")
load("Code/Application/Data/RawData/RCP85_DRY.Rdata")
load("Code/Application/Data/RawData/RCP26_WS.Rdata")
load("Code/Application/Data/RawData/RCP85_WS.Rdata")
#---------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
# ### RCP26.DRY
dd<- RCP26.FF.DRY

## Wind speed
RCP26.DRY.mean.wind.speed <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.wind.speed <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$fkl010h0
    }
    mean.wind.speed[m,] <- colMeans(temp)
  }
  
  RCP26.DRY.mean.wind.speed[[count]] <- mean.wind.speed
  count <- count+1
  
}

names(RCP26.DRY.mean.wind.speed) <- unique(dd$ID)


## Temperature
RCP26.DRY.mean.air.temp <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.air.temp <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$tre200h0
    }
    mean.air.temp[m,] <- colMeans(temp)
  }
  
  RCP26.DRY.mean.air.temp[[count]] <- mean.air.temp
  count <- count+1
  
}

names(RCP26.DRY.mean.air.temp) <- unique(dd$ID)

## Humidity
RCP26.DRY.mean.rel.hum  <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.rel.hum  <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$ure200h0
    }
    mean.rel.hum [m,] <- colMeans(temp)
  }
  
  RCP26.DRY.mean.rel.hum [[count]] <- mean.rel.hum
  count <- count+1
  
}

names(RCP26.DRY.mean.rel.hum ) <- unique(dd$ID)





#----------------------------------------------------------------------------------
### RCP26.WS
dd<- RCP26.FF.WS

## Wind speed
RCP26.WS.mean.wind.speed <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.wind.speed <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$fkl010h0
    }
    mean.wind.speed[m,] <- colMeans(temp)
  }
  
  RCP26.WS.mean.wind.speed[[count]] <- mean.wind.speed
  count <- count+1
  
}

names(RCP26.WS.mean.wind.speed) <- unique(dd$ID)



## Temperature
RCP26.WS.mean.air.temp <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.air.temp <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$tre200h0
    }
    mean.air.temp[m,] <- colMeans(temp)
  }
  
  RCP26.WS.mean.air.temp[[count]] <- mean.air.temp
  count <- count+1
  
}

names(RCP26.WS.mean.air.temp) <- unique(dd$ID)

## Humidity
RCP26.WS.mean.rel.hum  <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.rel.hum  <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$ure200h0
    }
    mean.rel.hum [m,] <- colMeans(temp)
  }
  
  RCP26.WS.mean.rel.hum [[count]] <- mean.rel.hum
  count <- count+1
  
}

names(RCP26.WS.mean.rel.hum ) <- unique(dd$ID)



#---------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
# ### RCP85.DRY
dd<- RCP85.FF.DRY

## Wind speed
RCP85.DRY.mean.wind.speed <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.wind.speed <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$fkl010h0
    }
    mean.wind.speed[m,] <- colMeans(temp)
  }
  
  RCP85.DRY.mean.wind.speed[[count]] <- mean.wind.speed
  count <- count+1
  
}

names(RCP85.DRY.mean.wind.speed) <- unique(dd$ID)


## Temperature
RCP85.DRY.mean.air.temp <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.air.temp <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$tre200h0
    }
    mean.air.temp[m,] <- colMeans(temp)
  }
  
  RCP85.DRY.mean.air.temp[[count]] <- mean.air.temp
  count <- count+1
  
}

names(RCP85.DRY.mean.air.temp) <- unique(dd$ID)

## Humidity
RCP85.DRY.mean.rel.hum  <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.rel.hum  <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$ure200h0
    }
    mean.rel.hum [m,] <- colMeans(temp)
  }
  
  RCP85.DRY.mean.rel.hum [[count]] <- mean.rel.hum
  count <- count+1
  
}

names(RCP85.DRY.mean.rel.hum ) <- unique(dd$ID)


#----------------------------------------------------------------------------------
### RCP85.WS
dd<- RCP85.FF.WS

## Wind speed
RCP85.WS.mean.wind.speed <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.wind.speed <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$fkl010h0
    }
    mean.wind.speed[m,] <- colMeans(temp)
  }
  
  RCP85.WS.mean.wind.speed[[count]] <- mean.wind.speed
  count <- count+1
  
}

names(RCP85.WS.mean.wind.speed) <- unique(dd$ID)



## Temperature
RCP85.WS.mean.air.temp <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.air.temp <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$tre200h0
    }
    mean.air.temp[m,] <- colMeans(temp)
  }
  
  RCP85.WS.mean.air.temp[[count]] <- mean.air.temp
  count <- count+1
  
}

names(RCP85.WS.mean.air.temp) <- unique(dd$ID)

## Humidity
RCP85.WS.mean.rel.hum  <- list()

count <- 1

for(station in unique(dd$ID)){
  ind0<- which(dd$ID == station)
  a0 <- dd[ind0,]
  mean.rel.hum  <- matrix(nrow=12, ncol=24)
  for(m in 1:12){
    ind.m <- which(a0$time.mm==m)
    a<- a0[ind.m,]
    max.d <- max(a$time.dd)
    temp <- matrix(nrow=max.d, ncol=24)
    for(d in 1:max.d){
      temp[d,] <-  a[which(a$time.dd==d),]$ure200h0
    }
    mean.rel.hum [m,] <- colMeans(temp)
  }
  
  RCP85.WS.mean.rel.hum [[count]] <- mean.rel.hum
  count <- count+1
  
}

names(RCP85.WS.mean.rel.hum ) <- unique(dd$ID)




#-----------------------------------------------------------------------------------

setwd("Code/Application/Data/Temperature")
save(RCP85.WS.mean.air.temp, file = "RCP85_WS_mean_air_temp.Rdata")
save(RCP26.WS.mean.air.temp, file = "RCP26_WS_mean_air_temp.Rdata")
save(RCP85.DRY.mean.air.temp, file = "RCP85_DRY_mean_air_temp.Rdata")
save(RCP26.DRY.mean.air.temp, file = "RCP26_DRY_mean_air_temp.Rdata")


setwd("Code/Application/Data/Humidity")
save(RCP85.WS.mean.rel.hum, file = "RCP85_WS_mean_rel_hum.Rdata")
save(RCP26.WS.mean.rel.hum, file = "RCP26_WS_mean_rel_hum.Rdata")
save(RCP85.DRY.mean.rel.hum, file = "RCP85_DRY_mean_rel_hum.Rdata")
save(RCP26.DRY.mean.rel.hum, file = "RCP26_DRY_mean_rel_hum.Rdata")



setwd("Code/Application/Data/Wind")
save(RCP85.WS.mean.wind.speed, file="RCP85_WS_mean_wind_speed.Rdata")
save(RCP26.WS.mean.wind.speed, file="RCP26_WS_mean_wind_speed.Rdata")
save(RCP85.DRY.mean.wind.speed, file="RCP85_DRY_mean_wind_speed.Rdata")
save(RCP26.DRY.mean.wind.speed, file="RCP26_DRY_mean_wind_speed.Rdata")


