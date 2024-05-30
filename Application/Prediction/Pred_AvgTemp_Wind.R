library(ExtrFunReg)
library(parallelDist)
library(ggplot2)
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

S <- 116
s <- seq(1,24, length.out=S)
shifts <- seq(-0.75, 0.75, by=0.05)
kappa <- 1
n.hF <- 20


#### Climate action

RCP26.WS.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.wind.speed.jul <- matrix(nrow=41, ncol=S)




for(i in 1:41){
  RCP26.WS.mean.wind.speed.jul[i,] <- RCP26.WS.mean.wind.speed.smooth[[i]][7,]
  RCP85.WS.mean.wind.speed.jul[i,] <- RCP85.WS.mean.wind.speed.smooth[[i]][7,]
  RCP26.DRY.mean.wind.speed.jul[i,] <- RCP26.DRY.mean.wind.speed.smooth[[i]][7,]
  RCP85.DRY.mean.wind.speed.jul[i,] <- RCP85.DRY.mean.wind.speed.smooth[[i]][7,]
  
}



### Compute the average curves and its shifts
avg.RCP26.WS <- colMeans(RCP26.WS.mean.wind.speed.jul)
avg.RCP85.WS <- colMeans(RCP85.WS.mean.wind.speed.jul)
avg.RCP26.DRY <- colMeans(RCP26.DRY.mean.wind.speed.jul)
avg.RCP85.DRY <- colMeans(RCP85.DRY.mean.wind.speed.jul)




curve.RCP26.WS <- curve.RCP85.WS <- curve.RCP26.DRY <- curve.RCP85.DRY  <- NULL
shift <- NULL
coord <- NULL
type.shift <- NULL

pred.RCP26.WS <- pred.RCP85.WS <- pred.RCP26.DRY <- pred.RCP85.DRY <- matrix(nrow = length(shifts), ncol=S)

for( i in 1:length(shifts)){
  curve.RCP26.WS <- c(curve.RCP26.WS, avg.RCP26.WS+shifts[i])
  curve.RCP85.WS <- c(curve.RCP85.WS, avg.RCP85.WS+shifts[i])
  curve.RCP26.DRY <- c(curve.RCP26.DRY, avg.RCP26.DRY+shifts[i])
  curve.RCP85.DRY <- c(curve.RCP85.DRY, avg.RCP85.DRY+shifts[i])
  shift <- c(shift, rep(shifts[i], S))
  coord <- c(coord, s)
  if(shifts[i]<0) type.shift <- c(type.shift, rep("Down",S))
  if(shifts[i]>0) type.shift <- c(type.shift, rep("Up",S))
  if(shifts[i]==0) type.shift <- c(type.shift, rep("Avg",S))
  pred.RCP26.WS[i,] <-avg.RCP26.WS+shifts[i]
  pred.RCP85.WS[i,] <-avg.RCP85.WS+shifts[i]
  pred.RCP26.DRY[i,] <-avg.RCP26.DRY+shifts[i]
  pred.RCP85.DRY[i,] <-avg.RCP85.DRY+shifts[i]
}


### Plot average curve and shifts

df <- data.frame(curve=c(curve.RCP26.DRY, curve.RCP85.DRY, curve.RCP26.WS, curve.RCP85.WS),
                 s=rep(coord,4),
                 shift=factor(rep(shift,4)),
                 type.shift=factor(rep(type.shift,4)),
                 action = factor(rep(c("RCP2.6 (Climate mitigation actions)", "RCP8.5 (No climate mitigation actions)", "RCP2.6 (Climate mitigation actions)", "RCP8.5 (No climate mitigation actions)"), each = length(curve.RCP26.DRY))),
                 scenario =  factor(rep(c("DRY", "WS"), each = 2*length(curve.RCP26.DRY))))


df$class <- ifelse(df$shift == "0", "Average", "Shift")



hour_labels <- c("12AM", paste(1:11, "AM"), "12PM", paste(1:11, "PM"))

p.shifts.july.wind <- ggplot(df, aes(x = s, y = curve, group = shift)) +
  geom_line(aes(color = class)) +
  scale_color_manual(values = c("Average" = "black", "Shift" = "grey70")) +
  labs(x = "Hour", y = expression(paste("Wind Speed  (", m*s^-1, ")", sep = "")), title = "Average Wind Speed in July with Shifts",
       color = "Type") +
  facet_grid(scenario ~ action) + theme(legend.position = "bottom") + scale_x_continuous(limits = c(1, 24), breaks = 1:24,  labels = hour_labels) +
  theme( legend.position = "none",
         plot.title = element_text(size = rel(2), face = "bold"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.title.y = element_text(size = rel(1.8)),
         axis.text.x = element_text(size = rel(1.5), angle = 90, hjust = 1),
         axis.text.y = element_text(size = rel(1.8)),
         strip.text.x = element_text(size = rel(1.5)),
         strip.text.y = element_text(size = rel(1.5)))

dev.new()
p.shifts.july.wind
dev.off()

#-----------------------------------------------------------------------------------
#### Compute predicted conditional extremiles
## July, RCP26 WS
res.RCP26.WS.jul.05.pred <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[7,]), RCP26.WS.mean.wind.speed.jul, pred.RCP26.WS, tau = 0.5, s=s, kappa=kappa,n.hF=n.hF)
hF <- res.RCP26.WS.jul.05.pred$hF
res.RCP26.WS.jul.07.pred <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[7,]), RCP26.WS.mean.wind.speed.jul, pred.RCP26.WS, tau = 0.7, s=s, hF=hF)
res.RCP26.WS.jul.08.pred <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[7,]), RCP26.WS.mean.wind.speed.jul, pred.RCP26.WS, tau = 0.8, s=s, hF=hF)
res.RCP26.WS.jul.09.pred <- ExtrFunReg(as.numeric(RCP26.WS.mean.air.temp.mat[7,]), RCP26.WS.mean.wind.speed.jul,  pred.RCP26.WS, tau = 0.9, s=s, hF=hF)


## July, RCP85 WS
res.RCP85.WS.jul.05.pred <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[7,]), RCP85.WS.mean.wind.speed.jul,pred.RCP26.WS, tau = 0.5, s=s, kappa=kappa,n.hF=n.hF)
hF <- res.RCP85.WS.jul.05.pred$hF
res.RCP85.WS.jul.07.pred <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[7,]), RCP85.WS.mean.wind.speed.jul, pred.RCP26.WS, tau = 0.7, s=s, hF=hF)
res.RCP85.WS.jul.08.pred <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[7,]), RCP85.WS.mean.wind.speed.jul, pred.RCP26.WS, tau = 0.8, s=s, hF=hF)
res.RCP85.WS.jul.09.pred <- ExtrFunReg(as.numeric(RCP85.WS.mean.air.temp.mat[7,]), RCP85.WS.mean.wind.speed.jul,  pred.RCP26.WS, tau = 0.9, s=s, hF=hF)


## July, RCP26 DRY
res.RCP26.DRY.jul.05.pred <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[7,]), RCP26.DRY.mean.wind.speed.jul, pred.RCP26.DRY, tau = 0.5, s=s, kappa=kappa,n.hF=n.hF)
hF <- res.RCP26.DRY.jul.05.pred$hF
res.RCP26.DRY.jul.07.pred <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[7,]), RCP26.DRY.mean.wind.speed.jul,  pred.RCP26.DRY, tau = 0.7, s=s, hF=hF)
res.RCP26.DRY.jul.08.pred <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[7,]), RCP26.DRY.mean.wind.speed.jul,  pred.RCP26.DRY, tau = 0.8, s=s, hF=hF)
res.RCP26.DRY.jul.09.pred <- ExtrFunReg(as.numeric(RCP26.DRY.mean.air.temp.mat[7,]), RCP26.DRY.mean.wind.speed.jul,   pred.RCP26.DRY, tau = 0.9, s=s, hF=hF)

## July, RCP85 DRY
res.RCP85.DRY.jul.05.pred <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[7,]), RCP85.DRY.mean.wind.speed.jul, pred.RCP85.DRY, tau = 0.5, s=s, kappa=kappa,n.hF=n.hF)
hF <- res.RCP85.DRY.jul.05.pred$hF
res.RCP85.DRY.jul.07.pred <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[7,]), RCP85.DRY.mean.wind.speed.jul,  pred.RCP85.DRY, tau = 0.7, s=s, hF=hF)
res.RCP85.DRY.jul.08.pred <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[7,]), RCP85.DRY.mean.wind.speed.jul,  pred.RCP85.DRY, tau = 0.8, s=s, hF=hF)
res.RCP85.DRY.jul.09.pred <- ExtrFunReg(as.numeric(RCP85.DRY.mean.air.temp.mat[7,]), RCP85.DRY.mean.wind.speed.jul,   pred.RCP85.DRY, tau = 0.9, s=s, hF=hF)


#### Plot predicted extremiles
res.RCP26.DRY.jul <-  c(res.RCP26.DRY.jul.05.pred$Predicted.extremiles,
                        res.RCP26.DRY.jul.07.pred$Predicted.extremiles,
                        res.RCP26.DRY.jul.08.pred$Predicted.extremiles,
                        res.RCP26.DRY.jul.09.pred$Predicted.extremiles)

res.RCP85.DRY.jul <-  c(res.RCP85.DRY.jul.05.pred$Predicted.extremiles,
                        res.RCP85.DRY.jul.07.pred$Predicted.extremiles,
                        res.RCP85.DRY.jul.08.pred$Predicted.extremiles,
                        res.RCP85.DRY.jul.09.pred$Predicted.extremiles)

res.RCP26.WS.jul <-  c(res.RCP26.WS.jul.05.pred$Predicted.extremiles,
                       res.RCP26.WS.jul.07.pred$Predicted.extremiles,
                       res.RCP26.WS.jul.08.pred$Predicted.extremiles,
                       res.RCP26.WS.jul.09.pred$Predicted.extremiles)

res.RCP85.WS.jul <-  c(res.RCP85.WS.jul.05.pred$Predicted.extremiles,
                       res.RCP85.WS.jul.07.pred$Predicted.extremiles,
                       res.RCP85.WS.jul.08.pred$Predicted.extremiles,
                       res.RCP85.WS.jul.09.pred$Predicted.extremiles)

df.extr <- data.frame(pred.xi = c(res.RCP26.DRY.jul, res.RCP85.DRY.jul,res.RCP26.WS.jul, res.RCP85.WS.jul),
                      shift=rep(shifts,4*4),
                      tau=factor(rep(rep(c(0.5, 0.7, 0.8,0.9), each=length(shifts)),4)),
                      action = factor(rep(c("RCP2.6 (Climate mitigation actions)", "RCP8.5 (No climate mitigation actions)", "RCP2.6 (Climate mitigation actions)", "RCP8.5 (No climate mitigation actions)"), each = 4*length(shifts))),
                      scenario =  factor(rep(c("DRY", "WS"), each = 2*4*length(shifts))))



colors <- colorRampPalette(c("black", "red"))(4)

dev.new()
ggplot(df.extr, aes(x = shift, y = pred.xi, colour = tau, group=tau)) +
  geom_line() +
  geom_point() + geom_vline(xintercept = 0, lty=2, col="grey")  +facet_grid(scenario ~ action) +
  scale_color_manual(values = colors, name=expression(tau)) + ylab(expression(hat(xi)[tau] ~ "- Temperature" ~ (degree*C))) +  xlab(expression(Delta^(Wind)))+
  labs(title = "Predicted extremiles of Average Temperature")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(2), face = "bold"),
        axis.title.x = element_text(size = rel(1.8)),
        axis.title.y = element_text(size = rel(1.8)),
        axis.text.x = element_text(size = rel(1.4)),
        axis.text.y = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(1.8)),
        legend.text = element_text(size = rel(1.8)),
        strip.text.x = element_text(size = rel(1.5)),
        strip.text.y = element_text(size = rel(1.5))
  )
dev.off()




