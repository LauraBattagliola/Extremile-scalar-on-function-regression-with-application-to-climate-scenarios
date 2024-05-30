library(ExtrFunReg)
library(qgam)
library(latex2exp)
library(ggplot2)
theme_set(theme_bw())
### Extract simulated datasets
B <- 500

my_seed=12334
set.seed(my_seed)
my_seeds <- round(runif(B,min=1, max=10^6), digits=0)
dat.g0 <- simData(n=200, tau=0.5, S=100, gamma=0, myseed=my_seeds[1], sd.eps=0.25 )
dat.g05 <- simData(n=200, tau=0.5, S=100, gamma=0.5, myseed=my_seeds[1], sd.eps=0.25 )
dat.g1 <- simData(n=200, tau=0.5, S=100, gamma=1, myseed=my_seeds[1], sd.eps=0.25 )
dat.g2 <- simData(n=200, tau=0.5, S=100, gamma=2, myseed=my_seeds[1], sd.eps=0.25 )


s <- seq(0,1, length.out=S)

### Extract the data for prediction, i.e. average temperature and its shifts
Xavg <- colMeans(dat.g0[[2]]$X)
shifts <- seq(-1, 1, by=0.05)

pred <- matrix(nrow=length(shifts), ncol=100)
for( i in 1:length(shifts)){
  pred[i,] <- Xavg + shifts[i]
}

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
## Extremile regression
kappa <- 1
n.hF <- 10

res.extr.01.g0 <- ExtrFunReg(dat.g0[[2]]$Y, dat[[2]]$X, pred, tau = 0.1, s=s, kappa=kappa,n.hF=n.hF)
hF=res.extr.01.g0$hF
res.extr.02.g0 <- ExtrFunReg(dat.g0[[2]]$Y, dat.g0[[2]]$X, pred, tau = 0.2, s=s, kappa=kappa,n.hF=n.hF, hF=hF)
res.extr.03.g0 <- ExtrFunReg(dat.g0[[2]]$Y, dat.g0[[2]]$X, pred, tau = 0.3, s=s, kappa=kappa,n.hF=n.hF, hF=hF)
res.extr.05.g0 <- ExtrFunReg(dat.g0[[2]]$Y, dat.g0[[2]]$X, pred, tau = 0.5, s=s, kappa=kappa,n.hF=n.hF, hF=hF)
res.extr.07.g0 <- ExtrFunReg(dat.g0[[2]]$Y, dat.g0[[2]]$X, pred, tau = 0.7, s=s, kappa=kappa,n.hF=n.hF, hF=hF)
res.extr.08.g0 <- ExtrFunReg(dat.g0[[2]]$Y, dat.g0[[2]]$X, pred, tau = 0.8, s=s, kappa=kappa,n.hF=n.hF, hF=hF)
res.extr.09.g0 <- ExtrFunReg(dat.g0[[2]]$Y, dat.g0[[2]]$X, pred, tau = 0.9, s=s, kappa=kappa,n.hF=n.hF, hF=hF)


extr.g0 <- c(res.extr.01.g0$Predicted.extremiles, res.extr.02.g0$Predicted.extremiles, res.extr.03.g0$Predicted.extremiles,
             res.extr.05.g0$Predicted.extremiles, res.extr.07.g0$Predicted.extremiles, res.extr.08.g0$Predicted.extremiles,
             res.extr.09.g0$Predicted.extremiles)



res.extr.01.g05 <- ExtrFunReg(dat.g05[[2]]$Y, dat.g05[[2]]$X, pred, tau = 0.1, s=s, kappa=kappa, n.hF=n.hF)
hF.g05 <- res.extr.01.g05$hF
res.extr.02.g05 <- ExtrFunReg(dat.g05[[2]]$Y, dat.g05[[2]]$X, pred, tau = 0.2, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g05)
res.extr.03.g05 <- ExtrFunReg(dat.g05[[2]]$Y, dat.g05[[2]]$X, pred, tau = 0.3, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g05)
res.extr.05.g05 <- ExtrFunReg(dat.g05[[2]]$Y, dat.g05[[2]]$X, pred, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g05)
res.extr.07.g05 <- ExtrFunReg(dat.g05[[2]]$Y, dat.g05[[2]]$X, pred, tau = 0.7, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g05)
res.extr.08.g05 <- ExtrFunReg(dat.g05[[2]]$Y, dat.g05[[2]]$X, pred, tau = 0.8, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g05)
res.extr.09.g05 <- ExtrFunReg(dat.g05[[2]]$Y, dat.g05[[2]]$X, pred, tau = 0.9, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g05)


extr.g05 <- c(res.extr.01.g05$Predicted.extremiles, res.extr.02.g05$Predicted.extremiles, res.extr.03.g05$Predicted.extremiles,
              res.extr.05.g05$Predicted.extremiles, res.extr.07.g05$Predicted.extremiles, res.extr.08.g05$Predicted.extremiles,
              res.extr.09.g05$Predicted.extremiles)


res.extr.01.g1 <- ExtrFunReg(dat.g1[[2]]$Y, dat.g1[[2]]$X, pred, tau = 0.1, s=s, kappa=kappa, n.hF=n.hF)
hF.g1 <- res.extr.01.g1$hF
res.extr.02.g1 <- ExtrFunReg(dat.g1[[2]]$Y, dat.g1[[2]]$X, pred, tau = 0.2, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g1)
res.extr.03.g1 <- ExtrFunReg(dat.g1[[2]]$Y, dat.g1[[2]]$X, pred, tau = 0.3, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g1)
res.extr.05.g1 <- ExtrFunReg(dat.g1[[2]]$Y, dat.g1[[2]]$X, pred, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g1)
res.extr.07.g1 <- ExtrFunReg(dat.g1[[2]]$Y, dat.g1[[2]]$X, pred, tau = 0.7, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g1)
res.extr.08.g1 <- ExtrFunReg(dat.g1[[2]]$Y, dat.g1[[2]]$X, pred, tau = 0.8, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g1)
res.extr.09.g1 <- ExtrFunReg(dat.g1[[2]]$Y, dat.g1[[2]]$X, pred, tau = 0.9, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g1)


extr.g1 <- c(res.extr.01.g1$Predicted.extremiles, res.extr.02.g1$Predicted.extremiles, res.extr.03.g1$Predicted.extremiles,
             res.extr.05.g1$Predicted.extremiles, res.extr.07.g1$Predicted.extremiles, res.extr.08.g1$Predicted.extremiles,
             res.extr.09.g1$Predicted.extremiles)



res.extr.01.g2 <- ExtrFunReg(dat.g2[[2]]$Y, dat.g2[[2]]$X, pred, tau = 0.1, s=s, kappa=kappa, n.hF=n.hF)
hF.g2 <- res.extr.01.g2$hF
res.extr.02.g2 <- ExtrFunReg(dat.g2[[2]]$Y, dat.g2[[2]]$X, pred, tau = 0.2, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g2)
res.extr.03.g2 <- ExtrFunReg(dat.g2[[2]]$Y, dat.g2[[2]]$X, pred, tau = 0.3, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g2)
res.extr.05.g2 <- ExtrFunReg(dat.g2[[2]]$Y, dat.g2[[2]]$X, pred, tau = 0.5, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g2)
res.extr.07.g2 <- ExtrFunReg(dat.g2[[2]]$Y, dat.g2[[2]]$X, pred, tau = 0.7, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g2)
res.extr.08.g2 <- ExtrFunReg(dat.g2[[2]]$Y, dat.g2[[2]]$X, pred, tau = 0.8, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g2)
res.extr.09.g2 <- ExtrFunReg(dat.g2[[2]]$Y, dat.g2[[2]]$X, pred, tau = 0.9, s=s, kappa=kappa, n.hF=n.hF, hF=hF.g2)


### Save results and parameters for data.frame later
extr.g2 <- c(res.extr.01.g2$Predicted.extremiles, res.extr.02.g2$Predicted.extremiles, res.extr.03.g2$Predicted.extremiles,
             res.extr.05.g2$Predicted.extremiles, res.extr.07.g2$Predicted.extremiles, res.extr.08.g2$Predicted.extremiles,
             res.extr.09.g2$Predicted.extremiles)


levels <- rep(rep(c(0.1,0.2,0.3,0.5,0.7,0.8,0.9), each=length(shifts)),4)

delta <- rep(rep(shifts, 7),4)

extr <- c(extr.g0, extr.g05, extr.g1, extr.g2)

gamma <- rep(c(0, 0.5, 1, 2), each = length(extr.g0))

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
## Quantile regression


sMat <- matrix(s, byrow = TRUE, nrow = 200, ncol=100)
formula <- as.formula(Y ~ s(sMat, by=X))

Y <- dat.g0[[2]]$Y
X <- dat.g0[[2]]$X

df.qgam <- data.frame(Y=Y)
df.qgam$X <- X
df.qgam$sMat <- sMat


res.quant.01.g0<- qgam(formula, qu=0.1, data=df.qgam)
res.quant.02.g0<- qgam(formula, qu=0.2, data=df.qgam)
res.quant.03.g0<- qgam(formula, qu=0.3, data=df.qgam)
res.quant.05.g0<- qgam(formula, qu=0.5, data=df.qgam)
res.quant.07.g0<- qgam(formula, qu=0.7, data=df.qgam)
res.quant.08.g0<- qgam(formula, qu=0.8, data=df.qgam)
res.quant.09.g0<- qgam(formula, qu=0.9, data=df.qgam)


Y <- dat.g05[[2]]$Y
X <- dat.g05[[2]]$X

df.qgam <- data.frame(Y=Y)
df.qgam$X <- X
df.qgam$sMat <- sMat


res.quant.01.g05<- qgam(formula, qu=0.1, data=df.qgam)
res.quant.02.g05<- qgam(formula, qu=0.2, data=df.qgam)
res.quant.03.g05<- qgam(formula, qu=0.3, data=df.qgam)
res.quant.05.g05<- qgam(formula, qu=0.5, data=df.qgam)
res.quant.07.g05<- qgam(formula, qu=0.7, data=df.qgam)
res.quant.08.g05<- qgam(formula, qu=0.8, data=df.qgam)
res.quant.09.g05<- qgam(formula, qu=0.9, data=df.qgam)


Y <- dat.g1[[2]]$Y
X <- dat.g1[[2]]$X

df.qgam <- data.frame(Y=Y)
df.qgam$X <- X
df.qgam$sMat <- sMat


res.quant.01.g1<- qgam(formula, qu=0.1, data=df.qgam)
res.quant.02.g1<- qgam(formula, qu=0.2, data=df.qgam)
res.quant.03.g1<- qgam(formula, qu=0.3, data=df.qgam)
res.quant.05.g1<- qgam(formula, qu=0.5, data=df.qgam)
res.quant.07.g1<- qgam(formula, qu=0.7, data=df.qgam)
res.quant.08.g1<- qgam(formula, qu=0.8, data=df.qgam)
res.quant.09.g1<- qgam(formula, qu=0.9, data=df.qgam)


Y <- dat.g2[[2]]$Y
X <- dat.g2[[2]]$X

df.qgam <- data.frame(Y=Y)
df.qgam$X <- X
df.qgam$sMat <- sMat


res.quant.01.g2<- qgam(formula, qu=0.1, data=df.qgam)
res.quant.02.g2<- qgam(formula, qu=0.2, data=df.qgam)
res.quant.03.g2<- qgam(formula, qu=0.3, data=df.qgam)
res.quant.05.g2<- qgam(formula, qu=0.5, data=df.qgam)
res.quant.07.g2<- qgam(formula, qu=0.7, data=df.qgam)
res.quant.08.g2<- qgam(formula, qu=0.8, data=df.qgam)
res.quant.09.g2<- qgam(formula, qu=0.9, data=df.qgam)


df.pred <- data.frame(shift=shifts)
df.pred$X <- pred
df.pred$sMat <- matrix(s, byrow = TRUE, nrow = length(shifts), ncol=100)

pred.quant.01.g0 <- pred.quant.02.g0 <- pred.quant.03.g0 <- pred.quant.05.g0 <- pred.quant.07.g0 <- pred.quant.08.g0 <- pred.quant.09.g0 <- rep(NA, length(shifts))
pred.quant.01.g05 <- pred.quant.02.g05 <- pred.quant.03.g05 <- pred.quant.05.g05 <- pred.quant.07.g05 <- pred.quant.08.g05 <- pred.quant.09.g05 <- rep(NA, length(shifts))
pred.quant.01.g1 <- pred.quant.02.g1 <- pred.quant.03.g1 <- pred.quant.05.g1 <- pred.quant.07.g1 <- pred.quant.08.g1 <- pred.quant.09.g1 <- rep(NA, length(shifts))
pred.quant.01.g2 <- pred.quant.02.g2 <- pred.quant.03.g2 <- pred.quant.05.g2 <- pred.quant.07.g2 <- pred.quant.08.g2 <- pred.quant.09.g2 <- rep(NA, length(shifts))

for(i in 1:length(shifts)){
  pred.quant.01.g0[i] <- mean(predict(res.quant.01.g0, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.02.g0[i] <- mean(predict(res.quant.02.g0, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.03.g0[i] <- mean(predict(res.quant.03.g0, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.05.g0[i] <- mean(predict(res.quant.05.g0, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.07.g0[i] <- mean(predict(res.quant.07.g0, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.08.g0[i] <- mean(predict(res.quant.08.g0, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.09.g0[i] <- mean(predict(res.quant.09.g0, df.pred[i,], newdata.guaranteed = TRUE))
  
  pred.quant.01.g05[i] <- mean(predict(res.quant.01.g05, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.02.g05[i] <- mean(predict(res.quant.02.g05, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.03.g05[i] <- mean(predict(res.quant.03.g05, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.05.g05[i] <- mean(predict(res.quant.05.g05, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.07.g05[i] <- mean(predict(res.quant.07.g05, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.08.g05[i] <- mean(predict(res.quant.08.g05, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.09.g05[i] <- mean(predict(res.quant.09.g05, df.pred[i,], newdata.guaranteed = TRUE))
  
  pred.quant.01.g1[i] <- mean(predict(res.quant.01.g1, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.02.g1[i] <- mean(predict(res.quant.02.g1, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.03.g1[i] <- mean(predict(res.quant.03.g1, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.05.g1[i] <- mean(predict(res.quant.05.g1, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.07.g1[i] <- mean(predict(res.quant.07.g1, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.08.g1[i] <- mean(predict(res.quant.08.g1, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.09.g1[i] <- mean(predict(res.quant.09.g1, df.pred[i,], newdata.guaranteed = TRUE))
  
  pred.quant.01.g2[i] <- mean(predict(res.quant.01.g2, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.02.g2[i] <- mean(predict(res.quant.02.g2, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.03.g2[i] <- mean(predict(res.quant.03.g2, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.05.g2[i] <- mean(predict(res.quant.05.g2, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.07.g2[i] <- mean(predict(res.quant.07.g2, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.08.g2[i] <- mean(predict(res.quant.08.g2, df.pred[i,], newdata.guaranteed = TRUE))
  pred.quant.09.g2[i] <- mean(predict(res.quant.09.g2, df.pred[i,], newdata.guaranteed = TRUE))
  
  
}

### Save results and parameters for data.frame later

quant.g0 <- c( pred.quant.01.g0,  pred.quant.02.g0,  pred.quant.03.g0,
               pred.quant.05.g0,  pred.quant.07.g0,  pred.quant.08.g0,
               pred.quant.09.g0)

quant.g05 <- c( pred.quant.01.g05,  pred.quant.02.g05,  pred.quant.03.g05,
                pred.quant.05.g05,  pred.quant.07.g05,  pred.quant.08.g05,
                pred.quant.09.g05)

quant.g1 <- c( pred.quant.01.g1,  pred.quant.02.g1,  pred.quant.03.g1,
               pred.quant.05.g1,  pred.quant.07.g1,  pred.quant.08.g1,
               pred.quant.09.g1)

quant.g2 <- c( pred.quant.01.g2,  pred.quant.02.g2,  pred.quant.03.g2,
               pred.quant.05.g2,  pred.quant.07.g2,  pred.quant.08.g2,
               pred.quant.09.g2)

quant <- c(quant.g0, quant.g05, quant.g1, quant.g2)

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
## Plot
df.all <- data.frame(res=c(extr, quant),
                     levels=factor(rep(levels,2)),
                     delta = rep(delta,2),
                     Regression = factor(rep(c("Extremile", "Quantile"), each=4*7*length(shifts))),
                     gamma = factor(rep(gamma,2)),
                     nb= factor(rep(1:2*4*7, each = length(shifts))))

theme_set(theme_bw())

colors.high <- colorRampPalette(c("black", "red"))(4)
colors.low <- colorRampPalette(c("blue", "black"))(4)

colors <- c(colors.low, colors.high[-1])


appender <- function(string) {
  TeX(paste("$\\gamma = $", string))
}

dev.new()
ggplot(df.all, aes(x = delta, y = res, colour = levels, lty = Regression)) +
  geom_line(lwd=1) +
  scale_color_manual(values = colors, name=expression(tau))+
  scale_linetype_manual(values = c("Quantile" = "dotted", "Extremile" = "solid"))+
  facet_grid(~gamma, labeller = as_labeller(appender,default = label_parsed))+
  labs(title = "Predicted Quantiles and Extremiles",
       x = expression(Delta)) +
  theme( legend.title=element_text(size = rel(1.8)), legend.position = "bottom",
         plot.title = element_text(size = rel(2), face = "bold"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.title.y = element_blank(),
         axis.text.x = element_text(size = rel(1.5)),
         axis.text.y = element_text(size = rel(1.8)),
         legend.text = element_text(size = rel(1.8)),
         strip.text.x = element_text(size = rel(1.5)),
         strip.text.y = element_text(size = rel(1.5)))

dev.off()

