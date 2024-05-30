library(ExtrFunReg)
library(parallelDist)
library(ggplot2)
theme_set(theme_bw())

### Load data and store it properly

load("/Code/Application/Data/Humidity/RCP26_DRY_mean_rel_hum.Rdata")
load("/Code/Application/Data/Humidity/RCP85_DRY_mean_rel_hum.Rdata")
load("/Code/Application/Data/Humidity/RCP26_WS_mean_rel_hum.Rdata")
load("/Code/Application/Data/Humidity/RCP85_WS_mean_rel_hum.Rdata")


RCP26.WS.mean.rel.hum.mat <- RCP26.DRY.mean.rel.hum.mat <- RCP85.WS.mean.rel.hum.mat <- RCP85.DRY.mean.rel.hum.mat <- matrix(nrow=12, ncol=41)

for(i in 1:41){
  RCP26.WS.mean.rel.hum.mat[,i] <- rowMeans(RCP26.WS.mean.rel.hum[[i]])
  RCP26.DRY.mean.rel.hum.mat[,i] <- rowMeans(RCP26.DRY.mean.rel.hum[[i]])
  RCP85.WS.mean.rel.hum.mat[,i] <- rowMeans(RCP85.WS.mean.rel.hum[[i]])
  RCP85.DRY.mean.rel.hum.mat[,i] <- rowMeans(RCP85.DRY.mean.rel.hum[[i]])
}


load("/Code/Application/Data/Temperature/RCP26_DRY_mean_air_temp_smooth.Rdata")
load("/Code/Application/Data/Temperature/RCP85_DRY_mean_air_temp_smooth.Rdata")
load("/Code/Application/Data/Temperature/RCP26_WS_mean_air_temp_smooth.Rdata")
load("/Code/Application/Data/Temperature/RCP85_WS_mean_air_temp_smooth.Rdata")

S <- 116
s <- seq(1,24, length.out=S)
shifts <- seq(-1.5, 1.5, length=31)
kappa <- 1
n.hF <- 20




RCP26.WS.mean.air.temp.jul <- matrix(nrow=41, ncol=S)
RCP85.WS.mean.air.temp.jul <- matrix(nrow=41, ncol=S)
RCP26.DRY.mean.air.temp.jul <- matrix(nrow=41, ncol=S)
RCP85.DRY.mean.air.temp.jul <- matrix(nrow=41, ncol=S)


for(i in 1:41){
  RCP26.WS.mean.air.temp.jul[i,] <- RCP26.WS.mean.air.temp.smooth[[i]][7,]
  RCP85.WS.mean.air.temp.jul[i,] <- RCP85.WS.mean.air.temp.smooth[[i]][7,]
  RCP26.DRY.mean.air.temp.jul[i,] <- RCP26.DRY.mean.air.temp.smooth[[i]][7,]
  RCP85.DRY.mean.air.temp.jul[i,] <- RCP85.DRY.mean.air.temp.smooth[[i]][7,]
  
}

### Compute the average curves and its shifts
avg.RCP26.WS <- colMeans(RCP26.WS.mean.air.temp.jul)
avg.RCP85.WS <- colMeans(RCP85.WS.mean.air.temp.jul)
avg.RCP26.DRY <- colMeans(RCP26.DRY.mean.air.temp.jul)
avg.RCP85.DRY <- colMeans(RCP85.DRY.mean.air.temp.jul)



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

p.shifts.july.temp <- ggplot(df, aes(x = s, y = curve, group = shift)) +
  geom_line(aes(color = class)) +
  scale_color_manual(values = c("Average" = "black", "Shift" = "grey70")) +
  labs(x = "Hour", y = expression(paste("Temperature  (", degree, "C)", sep = "")), title = "Average Temperature in July with Shifts",
       color = "Type") +
  facet_grid(scenario ~ action) + theme(legend.position = "none") + scale_x_continuous(limits = c(1, 24), breaks = 1:24,  labels = hour_labels) +
  theme( legend.position = "none",
         plot.title = element_text(size = rel(2), face = "bold"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.title.y = element_text(size = rel(1.8)),
         axis.text.x = element_text(size = rel(1.5), angle = 90, hjust = 1),
         axis.text.y = element_text(size = rel(1.8)),
         strip.text.x = element_text(size = rel(1.5)),
         strip.text.y = element_text(size = rel(1.5)))

dev.new()
p.shifts.july.temp
dev.off()

#-----------------------------------------------------------------------------------
#### Compute predicted conditional extremiles
## July, RCP26 WS
res.RCP26.WS.jul.05.pred <- ExtrFunReg(as.numeric(RCP26.WS.mean.rel.hum.mat[7,]), RCP26.WS.mean.air.temp.jul, pred.RCP26.WS, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.WS.jul.05.pred$hF
res.RCP26.WS.jul.03.pred <- ExtrFunReg(as.numeric(RCP26.WS.mean.rel.hum.mat[7,]), RCP26.WS.mean.air.temp.jul, pred.RCP26.WS, tau = 0.3, s=s, hF=hF)
res.RCP26.WS.jul.02.pred <- ExtrFunReg(as.numeric(RCP26.WS.mean.rel.hum.mat[7,]), RCP26.WS.mean.air.temp.jul, pred.RCP26.WS, tau = 0.2, s=s, hF=hF)
res.RCP26.WS.jul.01.pred <- ExtrFunReg(as.numeric(RCP26.WS.mean.rel.hum.mat[7,]), RCP26.WS.mean.air.temp.jul, pred.RCP26.WS, tau = 0.1, s=s, hF=hF)


## July, RCP85 WS
res.RCP85.WS.jul.05.pred <- ExtrFunReg(as.numeric(RCP85.WS.mean.rel.hum.mat[7,]), RCP85.WS.mean.air.temp.jul, pred.RCP85.WS, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.WS.jul.05.pred$hF
res.RCP85.WS.jul.03.pred <- ExtrFunReg(as.numeric(RCP85.WS.mean.rel.hum.mat[7,]), RCP85.WS.mean.air.temp.jul, pred.RCP85.WS, tau = 0.3, s=s, hF=hF)
res.RCP85.WS.jul.02.pred <- ExtrFunReg(as.numeric(RCP85.WS.mean.rel.hum.mat[7,]), RCP85.WS.mean.air.temp.jul, pred.RCP85.WS, tau = 0.2, s=s, hF=hF)
res.RCP85.WS.jul.01.pred <- ExtrFunReg(as.numeric(RCP85.WS.mean.rel.hum.mat[7,]), RCP85.WS.mean.air.temp.jul, pred.RCP85.WS, tau = 0.1, s=s, hF=hF)


## July, RCP26 DRY
res.RCP26.DRY.jul.05.pred <- ExtrFunReg(as.numeric(RCP26.DRY.mean.rel.hum.mat[7,]), RCP26.DRY.mean.air.temp.jul, pred.RCP26.DRY, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP26.DRY.jul.05.pred$hF
res.RCP26.DRY.jul.03.pred <- ExtrFunReg(as.numeric(RCP26.DRY.mean.rel.hum.mat[7,]), RCP26.DRY.mean.air.temp.jul, pred.RCP26.DRY, tau = 0.3, s=s, hF=hF)
res.RCP26.DRY.jul.02.pred <- ExtrFunReg(as.numeric(RCP26.DRY.mean.rel.hum.mat[7,]), RCP26.DRY.mean.air.temp.jul, pred.RCP26.DRY, tau = 0.2, s=s, hF=hF)
res.RCP26.DRY.jul.01.pred <- ExtrFunReg(as.numeric(RCP26.DRY.mean.rel.hum.mat[7,]), RCP26.DRY.mean.air.temp.jul,pred.RCP26.DRY, tau = 0.1, s=s, hF=hF)

## July, RCP85 DRY
res.RCP85.DRY.jul.05.pred <- ExtrFunReg(as.numeric(RCP85.DRY.mean.rel.hum.mat[7,]), RCP85.DRY.mean.air.temp.jul, pred.RCP85.DRY, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF)
hF <- res.RCP85.DRY.jul.05.pred$hF
res.RCP85.DRY.jul.03.pred <- ExtrFunReg(as.numeric(RCP85.DRY.mean.rel.hum.mat[7,]), RCP85.DRY.mean.air.temp.jul, pred.RCP85.DRY, tau = 0.3, s=s, hF=hF)
res.RCP85.DRY.jul.02.pred <- ExtrFunReg(as.numeric(RCP85.DRY.mean.rel.hum.mat[7,]), RCP85.DRY.mean.air.temp.jul, pred.RCP85.DRY, tau = 0.2, s=s, hF=hF)
res.RCP85.DRY.jul.01.pred <- ExtrFunReg(as.numeric(RCP85.DRY.mean.rel.hum.mat[7,]), RCP85.DRY.mean.air.temp.jul,pred.RCP85.DRY, tau = 0.1, s=s, hF=hF)



#### Plot predicted extremiles
res.RCP26.DRY.jul <-  c(res.RCP26.DRY.jul.05.pred$Predicted.extremiles,
                        res.RCP26.DRY.jul.03.pred$Predicted.extremiles,
                        res.RCP26.DRY.jul.02.pred$Predicted.extremiles,
                        res.RCP26.DRY.jul.01.pred$Predicted.extremiles)

res.RCP85.DRY.jul <-  c(res.RCP85.DRY.jul.05.pred$Predicted.extremiles,
                        res.RCP85.DRY.jul.03.pred$Predicted.extremiles,
                        res.RCP85.DRY.jul.02.pred$Predicted.extremiles,
                        res.RCP85.DRY.jul.01.pred$Predicted.extremiles)

res.RCP26.WS.jul <-  c(res.RCP26.WS.jul.05.pred$Predicted.extremiles,
                       res.RCP26.WS.jul.03.pred$Predicted.extremiles,
                       res.RCP26.WS.jul.02.pred$Predicted.extremiles,
                       res.RCP26.WS.jul.01.pred$Predicted.extremiles)

res.RCP85.WS.jul <-  c(res.RCP85.WS.jul.05.pred$Predicted.extremiles,
                       res.RCP85.WS.jul.03.pred$Predicted.extremiles,
                       res.RCP85.WS.jul.02.pred$Predicted.extremiles,
                       res.RCP85.WS.jul.01.pred$Predicted.extremiles)

df.extr <- data.frame(pred.xi = c(res.RCP26.DRY.jul, res.RCP85.DRY.jul,res.RCP26.WS.jul, res.RCP85.WS.jul),
                      shift=rep(shifts,4*4),
                      tau=factor(rep(rep(c(0.5, 0.3, 0.2,0.1), each=length(shifts)),4)),
                      action = factor(rep(c("RCP2.6 (Climate mitigation actions)", "RCP8.5 (No climate mitigation actions)", "RCP2.6 (Climate mitigation actions)", "RCP8.5 (No climate mitigation actions)"), each = 4*length(shifts))),
                      scenario =  factor(rep(c("DRY", "WS"), each = 2*4*length(shifts))))



colors <- colorRampPalette(c("blue", "black"))(4)


dev.new()
ggplot(df.extr, aes(x = shift, y = pred.xi, colour = tau, group=tau)) +
  geom_line() +
  geom_point() + geom_vline(xintercept = 0, lty=2, col="grey")  +facet_grid(scenario ~ action) +
  scale_color_manual(values = colors, name=expression(tau)) +ylab(expression(hat(xi)[tau] ~ "- Relative Humidity" ~ ("%")))+ xlab(expression(Delta^(Temp)))+
  labs(title="Predicted extremiles of Average Relative Humidity")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(2), face = "bold"),
        axis.title.x = element_text(size = rel(1.8)),
        axis.title.y = element_text(size = rel(1.8)),
        axis.text.x = element_text(size = rel(1.4)),
        axis.text.y = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(1.8)),
        legend.text = element_text(size = rel(1.8)),
        strip.text.x = element_text(size = rel(1.5)),
        strip.text.y = element_text(size = rel(1.5)))
dev.off()
