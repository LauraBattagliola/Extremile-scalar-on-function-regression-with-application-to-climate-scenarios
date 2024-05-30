library(ExtrFunReg)
library(parallelDist)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
get_legend <- function(my_plot) {
  tmp <- ggplotGrob(my_plot)
  leg <- gtable_filter(tmp, "guide-box")
  return(leg)
}
theme_set(theme_bw())
### Load data and store it properly

load("/Code/Application/Data/Temperature/RCP26_DRY_mean_air_temp.Rdata")
load("/Code/Application/Data/Temperature/RCP85_DRY_mean_air_temp.Rdata")
load("/Code/Application/Data/Temperature/RCP26_WS_mean_air_temp.Rdata")
load("/Code/Application/Data/Temperature/RCP85_WS_mean_air_temp.Rdata")

RCP26.DRY.mean.air.temp.mat <- RCP85.DRY.mean.air.temp.mat <-  RCP26.WS.mean.air.temp.mat <-  RCP85.WS.mean.air.temp.mat <- matrix(nrow=12, ncol=41)

for(i in 1:41){
  RCP26.DRY.mean.air.temp.mat[,i] <- rowMeans(RCP26.DRY.mean.air.temp[[i]])
  RCP85.DRY.mean.air.temp.mat[,i] <- rowMeans(RCP85.DRY.mean.air.temp[[i]])
  RCP26.WS.mean.air.temp.mat[,i] <- rowMeans(RCP26.WS.mean.air.temp[[i]])
  RCP85.WS.mean.air.temp.mat[,i] <- rowMeans(RCP85.WS.mean.air.temp[[i]])
}





load("/Code/Application/Data/Wind/RCP26_DRY_mean_wind_speed_smooth.Rdata")
load("/Code/Application/Data/Wind/RCP85_DRY_mean_wind_speed_smooth.Rdata")
load("/Code/Application/Data/Wind/RCP26_WS_mean_wind_speed_smooth.Rdata")
load("/Code/Application/Data/Wind/RCP85_WS_mean_wind_speed_smooth.Rdata")

### Common parameters of the models

S <- 116
s <- seq(1,24, length.out=S)
kappa <- 4
n.hF <- 20

#############################################################################################
#############################################################################################
#############################################################################################
####### Warm summer (WS)

#### Climate action taken (RCP2.6)
RCP26.WS.mean.wind.speed.jun <- matrix(nrow=41, ncol=S)
RCP26.WS.mean.wind.speed.may <- matrix(nrow=41, ncol=S)
RCP26.WS.mean.wind.speed.sept <- matrix(nrow=41, ncol=S)
RCP26.WS.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP26.WS.mean.wind.speed.aug <- matrix(nrow=41, ncol=S)



for(i in 1:41){
  RCP26.WS.mean.wind.speed.jun[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][6,]
  RCP26.WS.mean.wind.speed.may[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][5,]
  RCP26.WS.mean.wind.speed.sept[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][9,]
  RCP26.WS.mean.wind.speed.jul[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][7,]
  RCP26.WS.mean.wind.speed.aug[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][8,]
}




## May
res.RCP26.WS.may.05 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[5,]), RCP26.WS.mean.wind.speed.may, RCP26.WS.mean.wind.speed.may, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.WS.may.05$hF
res.RCP26.WS.may.09 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[5,]), RCP26.WS.mean.wind.speed.may, RCP26.WS.mean.wind.speed.may, tau = 0.9, s=s, hF=hF)

## June
res.RCP26.WS.jun.05 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[6,]), RCP26.WS.mean.wind.speed.jun, RCP26.WS.mean.wind.speed.jun, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.WS.jun.05$hF
res.RCP26.WS.jun.09 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[6,]), RCP26.WS.mean.wind.speed.jun, RCP26.WS.mean.wind.speed.jun, tau = 0.9, s=s, hF=hF)


## July
res.RCP26.WS.jul.05 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[7,]), RCP26.WS.mean.wind.speed.jul, RCP26.WS.mean.wind.speed.jul, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.WS.jul.05$hF
res.RCP26.WS.jul.09 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[7,]), RCP26.WS.mean.wind.speed.jul, RCP26.WS.mean.wind.speed.jul, tau = 0.9, s=s, hF=hF)


## August
res.RCP26.WS.aug.05 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[8,]), RCP26.WS.mean.wind.speed.aug, RCP26.WS.mean.wind.speed.aug, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.WS.aug.05$hF
res.RCP26.WS.aug.09 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[8,]), RCP26.WS.mean.wind.speed.aug, RCP26.WS.mean.wind.speed.aug, tau = 0.9, s=s, hF=hF)


## September
res.RCP26.WS.sept.05 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[9,]), RCP26.WS.mean.wind.speed.sept, RCP26.WS.mean.wind.speed.sept, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.WS.sept.05$hF
res.RCP26.WS.sept.09 <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[9,]), RCP26.WS.mean.wind.speed.sept, RCP26.WS.mean.wind.speed.sept, tau = 0.9, s=s, hF=hF)




#--------------------------------------------------------------------------------
#### No climate action taken (RCP8.5)
RCP85.WS.mean.wind.speed.jun <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.wind.speed.may <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.wind.speed.sept <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.wind.speed.aug <- matrix(nrow=41, ncol=S)



for(i in 1:41){
  RCP85.WS.mean.wind.speed.jun[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][6,]
  RCP85.WS.mean.wind.speed.may[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][5,]
  RCP85.WS.mean.wind.speed.sept[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][9,]
  RCP85.WS.mean.wind.speed.jul[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][7,]
  RCP85.WS.mean.wind.speed.aug[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][8,]
}



## May
res.RCP85.WS.may.05 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[5,]), RCP85.WS.mean.wind.speed.may, RCP85.WS.mean.wind.speed.may, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.WS.may.05$hF
res.RCP85.WS.may.09 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[5,]), RCP85.WS.mean.wind.speed.may, RCP85.WS.mean.wind.speed.may, tau = 0.9, s=s, hF=hF)

## June
res.RCP85.WS.jun.05 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[6,]), RCP85.WS.mean.wind.speed.jun, RCP85.WS.mean.wind.speed.jun, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.WS.jun.05$hF
res.RCP85.WS.jun.09 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[6,]), RCP85.WS.mean.wind.speed.jun, RCP85.WS.mean.wind.speed.jun, tau = 0.9, s=s, hF=hF)


## July
res.RCP85.WS.jul.05 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[7,]), RCP85.WS.mean.wind.speed.jul, RCP85.WS.mean.wind.speed.jul, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.WS.jul.05$hF
res.RCP85.WS.jul.09 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[7,]), RCP85.WS.mean.wind.speed.jul, RCP85.WS.mean.wind.speed.jul, tau = 0.9, s=s, hF=hF)

## August
res.RCP85.WS.aug.05 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[8,]), RCP85.WS.mean.wind.speed.aug, RCP85.WS.mean.wind.speed.aug, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.WS.aug.05$hF
res.RCP85.WS.aug.09 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[8,]), RCP85.WS.mean.wind.speed.aug, RCP85.WS.mean.wind.speed.aug, tau = 0.9, s=s, hF=hF)


## September
res.RCP85.WS.sept.05 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[9,]), RCP85.WS.mean.wind.speed.sept, RCP85.WS.mean.wind.speed.sept, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.WS.may.05$hF
res.RCP85.WS.sept.09 <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[9,]), RCP85.WS.mean.wind.speed.sept, RCP85.WS.mean.wind.speed.sept, tau = 0.9, s=s, hF=hF)


######################################################################################
######################################################################################
######################################################################################

##### Reference year (DRY)

### Climate actions taken (RCP2.6)


RCP26.DRY.mean.wind.speed.jun <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.wind.speed.may <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.wind.speed.sept <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.wind.speed.aug <- matrix(nrow=41, ncol=S)

for(i in 1:41){
  RCP26.DRY.mean.wind.speed.jun[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][6,]
  RCP26.DRY.mean.wind.speed.may[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][5,]
  RCP26.DRY.mean.wind.speed.sept[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][9,]
  RCP26.DRY.mean.wind.speed.jul[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][7,]
  RCP26.DRY.mean.wind.speed.aug[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][8,]
}

## May
res.RCP26.DRY.may.05 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[5,]), RCP26.DRY.mean.wind.speed.may, RCP26.DRY.mean.wind.speed.may, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.DRY.may.05$hF
res.RCP26.DRY.may.09 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[5,]), RCP26.DRY.mean.wind.speed.may, RCP26.DRY.mean.wind.speed.may, tau = 0.9, s=s, hF=hF)

## June
res.RCP26.DRY.jun.05 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[6,]), RCP26.DRY.mean.wind.speed.jun, RCP26.DRY.mean.wind.speed.jun, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.DRY.jun.05$hF
res.RCP26.DRY.jun.09 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[6,]), RCP26.DRY.mean.wind.speed.jun, RCP26.DRY.mean.wind.speed.jun, tau = 0.9, s=s, hF=hF)


## July
res.RCP26.DRY.jul.05 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[7,]), RCP26.DRY.mean.wind.speed.jul, RCP26.DRY.mean.wind.speed.jul, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.DRY.jul.05$hF
res.RCP26.DRY.jul.09 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[7,]), RCP26.DRY.mean.wind.speed.jul, RCP26.DRY.mean.wind.speed.jul, tau = 0.9, s=s, hF=hF)

## August
res.RCP26.DRY.aug.05 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[8,]), RCP26.DRY.mean.wind.speed.aug, RCP26.DRY.mean.wind.speed.aug, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.DRY.aug.05$hF
res.RCP26.DRY.aug.09 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[8,]), RCP26.DRY.mean.wind.speed.aug, RCP26.DRY.mean.wind.speed.aug, tau = 0.9, s=s, hF=hF)

## September
res.RCP26.DRY.sept.05 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[9,]), RCP26.DRY.mean.wind.speed.sept, RCP26.DRY.mean.wind.speed.sept, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.DRY.sept.05$hF
res.RCP26.DRY.sept.09 <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[9,]), RCP26.DRY.mean.wind.speed.sept, RCP26.DRY.mean.wind.speed.sept, tau = 0.9, s=s, hF=hF)

#-----------------------------
## No climate actions taken (RCP8.5)
RCP85.DRY.mean.wind.speed.jun <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.wind.speed.may <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.wind.speed.sept <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.wind.speed.aug <- matrix(nrow=41, ncol=S)

for(i in 1:41){
  RCP85.DRY.mean.wind.speed.jun[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][6,]
  RCP85.DRY.mean.wind.speed.may[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][5,]
  RCP85.DRY.mean.wind.speed.sept[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][9,]
  RCP85.DRY.mean.wind.speed.jul[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][7,]
  RCP85.DRY.mean.wind.speed.aug[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][8,]
}


## May
res.RCP85.DRY.may.05 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[5,]), RCP85.DRY.mean.wind.speed.may, RCP85.DRY.mean.wind.speed.may, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.DRY.may.05$hF
res.RCP85.DRY.may.09 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[5,]), RCP85.DRY.mean.wind.speed.may, RCP85.DRY.mean.wind.speed.may, tau = 0.9, s=s, hF=hF)



## June
res.RCP85.DRY.jun.05 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[6,]), RCP85.DRY.mean.wind.speed.jun, RCP85.DRY.mean.wind.speed.jun, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.DRY.jun.05$hF
res.RCP85.DRY.jun.09 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[6,]), RCP85.DRY.mean.wind.speed.jun, RCP85.DRY.mean.wind.speed.jun, tau = 0.9, s=s, hF=hF)



## July
res.RCP85.DRY.jul.05 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[7,]), RCP85.DRY.mean.wind.speed.jul, RCP85.DRY.mean.wind.speed.jul, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.DRY.jul.05$hF
res.RCP85.DRY.jul.09 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[7,]), RCP85.DRY.mean.wind.speed.jul, RCP85.DRY.mean.wind.speed.jul, tau = 0.9, s=s, hF=hF)



## August
res.RCP85.DRY.aug.05 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[8,]), RCP85.DRY.mean.wind.speed.aug, RCP85.DRY.mean.wind.speed.aug, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.DRY.aug.05$hF
res.RCP85.DRY.aug.09 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[8,]), RCP85.DRY.mean.wind.speed.aug, RCP85.DRY.mean.wind.speed.aug, tau = 0.9, s=s, hF=hF)



## September
res.RCP85.DRY.sept.05 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[9,]), RCP85.DRY.mean.wind.speed.sept, RCP85.DRY.mean.wind.speed.sept, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.DRY.sept.05$hF
res.RCP85.DRY.sept.09 <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[9,]), RCP85.DRY.mean.wind.speed.sept, RCP85.DRY.mean.wind.speed.sept, tau = 0.9, s=s, hF=hF)



###################################################################################
###################################################################################
###################################################################################
###################################################################################

### Plots

df.temp.wind.dry <- data.frame(Station = factor(rep(c("ABO", "AIG", "ALT", "BAS", "BER", "BKLI", "BUS", "CDF", "CHU", "DAV", "DIS", "ENG",
                                                      "FRE", "GLA", "GSB", "GUT", "GVE", "INT", "KLO", "LUG", "LUZ", "MAG", "MVE", "NABBER",
                                                      "NABLAU", "OTL", "PIO", "PUY", "REH", "ROB", "RUE", "SAM", "SBE", "SCU", "SHA", "SIO",
                                                      "STG", "ULR", "VAD", "WYN", "ZER"), 5*4)),
                               Scenario = factor(rep(c(rep("RCP2.6 (Climate mitigation actions)", 41*5), rep("RCP8.5 (No climate mitigation actions)", 41*5)),2)),
                               Month = factor(rep(rep(5:9, each=41),4), ordered = TRUE, labels = c("May","June", "July", "August", "September")),
                               Extremiles = c(
                                 # RCP2.6, level 0.5
                                 res.RCP26.DRY.may.05$Estimated.extremiles, res.RCP26.DRY.jun.05$Estimated.extremiles,
                                 res.RCP26.DRY.jul.05$Estimated.extremiles, res.RCP26.DRY.aug.05$Estimated.extremiles, res.RCP26.DRY.sept.05$Estimated.extremiles,
                                 
                                 
                                 # RCP8.5, level 0.5
                                 res.RCP85.DRY.may.05$Estimated.extremiles, res.RCP85.DRY.jun.05$Estimated.extremiles,
                                 res.RCP85.DRY.jul.05$Estimated.extremiles, res.RCP85.DRY.aug.05$Estimated.extremiles, res.RCP85.DRY.sept.05$Estimated.extremiles,
                                 
                                 
                                 # RCP2.6, level 0.9
                                 res.RCP26.DRY.may.09$Estimated.extremiles, res.RCP26.DRY.jun.09$Estimated.extremiles,
                                 res.RCP26.DRY.jul.09$Estimated.extremiles, res.RCP26.DRY.aug.09$Estimated.extremiles, res.RCP26.DRY.sept.09$Estimated.extremiles,
                                 
                                 
                                 # RCP8.5, level 0.9
                                 res.RCP85.DRY.may.09$Estimated.extremiles, res.RCP85.DRY.jun.09$Estimated.extremiles,
                                 res.RCP85.DRY.jul.09$Estimated.extremiles, res.RCP85.DRY.aug.09$Estimated.extremiles, res.RCP85.DRY.sept.09$Estimated.extremiles
                                 
                               )
                               ,
                               Level = factor(rep(c("0.5", "0.9"),each=41*5*2)))




df.temp.wind.ws <- data.frame(Station = factor(rep(c("ABO", "AIG", "ALT", "BAS", "BER", "BKLI", "BUS", "CDF", "CHU", "DAV", "DIS", "ENG",
                                                     "FRE", "GLA", "GSB", "GUT", "GVE", "INT", "KLO", "LUG", "LUZ", "MAG", "MVE", "NABBER",
                                                     "NABLAU", "OTL", "PIO", "PUY", "REH", "ROB", "RUE", "SAM", "SBE", "SCU", "SHA", "SIO",
                                                     "STG", "ULR", "VAD", "WYN", "ZER"), 5*4)),
                              Scenario = factor(rep(c(rep("RCP2.6 (Climate mitigation actions)", 41*5), rep("RCP8.5 (No climate mitigation actions)", 41*5)),2)),
                              Month = factor(rep(rep(5:9, each=41),4), ordered = TRUE, labels = c("May","June", "July", "August", "September")),
                              Extremiles = c(
                                # RCP2.6, level 0.5
                                res.RCP26.WS.may.05$Estimated.extremiles, res.RCP26.WS.jun.05$Estimated.extremiles,
                                res.RCP26.WS.jul.05$Estimated.extremiles, res.RCP26.WS.aug.05$Estimated.extremiles, res.RCP26.WS.sept.05$Estimated.extremiles,
                                
                                
                                # RCP8.5, level 0.5
                                res.RCP85.WS.may.05$Estimated.extremiles, res.RCP85.WS.jun.05$Estimated.extremiles,
                                res.RCP85.WS.jul.05$Estimated.extremiles, res.RCP85.WS.aug.05$Estimated.extremiles, res.RCP85.WS.sept.05$Estimated.extremiles,
                                
                                
                                # RCP2.6, level 0.9
                                res.RCP26.WS.may.09$Estimated.extremiles, res.RCP26.WS.jun.09$Estimated.extremiles,
                                res.RCP26.WS.jul.09$Estimated.extremiles, res.RCP26.WS.aug.09$Estimated.extremiles, res.RCP26.WS.sept.09$Estimated.extremiles,
                                
                                
                                # RCP8.5, level 0.9
                                res.RCP85.WS.may.09$Estimated.extremiles, res.RCP85.WS.jun.09$Estimated.extremiles,
                                res.RCP85.WS.jul.09$Estimated.extremiles, res.RCP85.WS.aug.09$Estimated.extremiles, res.RCP85.WS.sept.09$Estimated.extremiles
                                
                              )
                              ,
                              Level = factor(rep(c("0.5", "0.9"),each=41*5*2)))



library(viridis)
my_col<- viridis(3)



p.temp.wind.dry <- ggplot(df.temp.wind.dry, aes(x=Station, y = Extremiles, color=Level )) + geom_point(pch=1, size=3,stat = "identity", stroke=1) + facet_grid(Month ~  Scenario) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +labs(y = expression(hat(xi)[tau] ~ "-" ~ "Monthly Average Temperature" ~ (degree*C)), color = expression(tau))+
  scale_color_manual(values = c("black", "red"))+
  geom_vline(xintercept = "STG", linetype = "dashed", color =my_col[1], lwd=1) +
  geom_vline(xintercept = "LUG", linetype = "dashed", color =my_col[2], lwd=1) +
  geom_vline(xintercept = "REH", linetype = "dashed", color =my_col[3], lwd=1)+
  scale_x_discrete(breaks = c("STG", "LUG", "REH"), labels = c( "St. Gallen", "Lugano",  "Zurich"  ))


p.temp.wind.ws <- ggplot(df.temp.wind.ws, aes(x=Station, y = Extremiles, color=Level )) + geom_point(pch=1, size=3,stat = "identity", stroke=1) + facet_grid(Month ~  Scenario) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=45, hjust=1, vjust=1)) + labs(y = expression(hat(xi)[tau] ~ "-" ~ "Monthly Average Temperature" ~ (degree*C)), color = expression(tau))+
  scale_color_manual(values = c("black", "red")) +
  geom_vline(xintercept = "STG", linetype = "dashed", color =my_col[1], lwd=1) +
  geom_vline(xintercept = "LUG", linetype = "dashed", color =my_col[2], lwd=1) +
  geom_vline(xintercept = "REH", linetype = "dashed", color =my_col[3], lwd=1)+
  scale_x_discrete(breaks = c("STG", "LUG", "REH"), labels = c( "St. Gallen", "Lugano",  "Zurich"  ))


min.extr <-min(min(df.temp.wind.ws$Extremiles), min(df.temp.wind.dry$Extremiles))
max.extr <-max(max(df.temp.wind.ws$Extremiles), max(df.temp.wind.dry$Extremiles))



p.temp.wind.ws <- p.temp.wind.ws + labs(title = "Estimated extremiles in Warm Summer Case")  + ylim(c(min.extr, max.extr)) +
  theme(
    plot.title = element_text(size = rel(2), face = "bold"),
    axis.title.x = element_text(size = rel(1.8)),
    axis.title.y = element_text(size = rel(1.8)),
    axis.text.x = element_text(size = rel(1.8)),
    axis.text.y = element_text(size = rel(1.8)),
    legend.title = element_text(size = rel(1.8)),
    legend.text = element_text(size = rel(1.8)), #
    strip.text.x = element_text(size = rel(1.5)),
    strip.text.y = element_text(size = rel(1.5))
  )
p.temp.wind.dry <- p.temp.wind.dry  + labs(title = "Estimated extremiles in Reference Year Case") + ylim(c(min.extr, max.extr)) +
  theme(
    plot.title = element_text(size = rel(2), face = "bold"), # Adjust plot title
    axis.title.x = element_text(size = rel(1.8)),
    axis.title.y = element_text(size = rel(1.8)),
    axis.text.x = element_text(size = rel(1.8)),
    axis.text.y = element_text(size = rel(1.8)),
    legend.title = element_text(size = rel(1.8)),
    legend.text = element_text(size = rel(1.8)),
    strip.text.x = element_text(size = rel(1.5)),
    strip.text.y = element_text(size = rel(1.5))
  )


legend <- get_legend( p.temp.wind.ws)

dev.new()
grid.arrange(
  arrangeGrob(p.temp.wind.dry + theme(legend.position = "none"),
              p.temp.wind.ws + theme(legend.position = "none"), ncol = 2),
  legend,
  heights = c(9/10, 1/10) 
)
dev.off()

#----------------------------------------------------------
### Table for the month of July
library(xtable)
## DRY case
ind.lug <- which( df.temp.wind.dry$Station==c("LUG")  & df.temp.wind.dry$Month=="July")
ind.zur <- which( df.temp.wind.dry$Station==c("REH")  & df.temp.wind.dry$Month=="July")
ind.stg <- which( df.temp.wind.dry$Station==c("STG")  & df.temp.wind.dry$Month=="July")

ind <- c(ind.lug,ind.zur, ind.stg )

xtable(df.temp.wind.dry[ind,])

## WS case
ind.lug <- which( df.temp.wind.ws$Station==c("LUG")  & df.temp.wind.ws$Month=="July")
ind.zur <- which( df.temp.wind.ws$Station==c("REH")  & df.temp.wind.ws$Month=="July")
ind.stg <- which( df.temp.wind.ws$Station==c("STG")  & df.temp.wind.ws$Month=="July")

ind <- c(ind.lug,ind.zur, ind.stg )

xtable(df.temp.wind.ws[ind,])


