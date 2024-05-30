library(ggplot2)
library(latex2exp)
theme_set(theme_bw())

################################################################################
############### Comparison of MSE for different values of kappa ################
################################################################################

load("/Code/Simulations/Data/kappa1/ss_g0_tau1.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g0_tau2.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g0_tau3.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g0_tau5.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g0_tau7.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g0_tau8.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g0_tau9.Rdata")

load("/Code/Simulations/Data/kappa1/ss_g05_tau1.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g05_tau2.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g05_tau3.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g05_tau5.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g05_tau7.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g05_tau8.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g05_tau9.Rdata")

load("/Code/Simulations/Data/kappa1/ss_g1_tau1.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g1_tau2.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g1_tau3.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g1_tau5.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g1_tau7.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g1_tau8.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g1_tau9.Rdata")

load("/Code/Simulations/Data/kappa1/ss_g2_tau1.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g2_tau2.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g2_tau3.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g2_tau5.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g2_tau7.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g2_tau8.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g2_tau9.Rdata")

ss1.k1 <- ss1
ss2.k1 <- ss2
ss3.k1 <- ss3
ss5.k1 <- ss5
ss7.k1 <- ss7
ss8.k1 <- ss8
ss9.k1 <- ss9

ss1.g05.k1 <- ss1.g05
ss2.g05.k1 <- ss2.g05
ss3.g05.k1 <- ss3.g05
ss5.g05.k1 <- ss5.g05
ss7.g05.k1 <- ss7.g05
ss8.g05.k1 <- ss8.g05
ss9.g05.k1 <- ss9.g05

ss1.g1.k1 <- ss1.g1
ss2.g1.k1 <- ss2.g1
ss3.g1.k1 <- ss3.g1
ss5.g1.k1 <- ss5.g1
ss7.g1.k1 <- ss7.g1
ss8.g1.k1 <- ss8.g1
ss9.g1.k1 <- ss9.g1

ss1.g2.k1 <- ss1.g2
ss2.g2.k1 <- ss2.g2
ss3.g2.k1 <- ss3.g2
ss5.g2.k1 <- ss5.g2
ss7.g2.k1 <- ss7.g2
ss8.g2.k1 <- ss8.g2
ss9.g2.k1 <- ss9.g2


load("/Code/Simulations/Data/kappa4/ss_g0_tau1.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g0_tau2.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g0_tau3.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g0_tau5.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g0_tau7.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g0_tau8.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g0_tau9.Rdata")

load("/Code/Simulations/Data/kappa4/ss_g05_tau1.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g05_tau2.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g05_tau3.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g05_tau5.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g05_tau7.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g05_tau8.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g05_tau9.Rdata")

load("/Code/Simulations/Data/kappa4/ss_g1_tau1.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g1_tau2.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g1_tau3.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g1_tau5.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g1_tau7.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g1_tau8.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g1_tau9.Rdata")

load("/Code/Simulations/Data/kappa4/ss_g2_tau1.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g2_tau2.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g2_tau3.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g2_tau5.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g2_tau7.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g2_tau8.Rdata")
load("/Code/Simulations/Data/kappa4/ss_g2_tau9.Rdata")

ss1.k4 <- ss1
ss2.k4 <- ss2
ss3.k4 <- ss3
ss5.k4 <- ss5
ss7.k4 <- ss7
ss8.k4 <- ss8
ss9.k4 <- ss9

ss1.g05.k4 <- ss1.g05
ss2.g05.k4 <- ss2.g05
ss3.g05.k4 <- ss3.g05
ss5.g05.k4 <- ss5.g05
ss7.g05.k4 <- ss7.g05
ss8.g05.k4 <- ss8.g05
ss9.g05.k4 <- ss9.g05

ss1.g1.k4 <- ss1.g1
ss2.g1.k4 <- ss2.g1
ss3.g1.k4 <- ss3.g1
ss5.g1.k4 <- ss5.g1
ss7.g1.k4 <- ss7.g1
ss8.g1.k4 <- ss8.g1
ss9.g1.k4 <- ss9.g1

ss1.g2.k4 <- ss1.g2
ss2.g2.k4 <- ss2.g2
ss3.g2.k4 <- ss3.g2
ss5.g2.k4 <- ss5.g2
ss7.g2.k4 <- ss7.g2
ss8.g2.k4 <- ss8.g2
ss9.g2.k4 <- ss9.g2


load("/Code/Simulations/Data/kappa05/ss_g0_tau1.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g0_tau2.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g0_tau3.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g0_tau5.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g0_tau7.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g0_tau8.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g0_tau9.Rdata")

load("/Code/Simulations/Data/kappa05/ss_g05_tau1.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g05_tau2.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g05_tau3.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g05_tau5.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g05_tau7.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g05_tau8.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g05_tau9.Rdata")

load("/Code/Simulations/Data/kappa05/ss_g1_tau1.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g1_tau2.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g1_tau3.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g1_tau5.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g1_tau7.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g1_tau8.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g1_tau9.Rdata")

load("/Code/Simulations/Data/kappa05/ss_g2_tau1.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g2_tau2.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g2_tau3.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g2_tau5.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g2_tau7.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g2_tau8.Rdata")
load("/Code/Simulations/Data/kappa05/ss_g2_tau9.Rdata")

ss1.k05 <- ss1
ss2.k05 <- ss2
ss3.k05 <- ss3
ss5.k05 <- ss5
ss7.k05 <- ss7
ss8.k05 <- ss8
ss9.k05 <- ss9

ss1.g05.k05 <- ss1.g05
ss2.g05.k05 <- ss2.g05
ss3.g05.k05 <- ss3.g05
ss5.g05.k05 <- ss5.g05
ss7.g05.k05 <- ss7.g05
ss8.g05.k05 <- ss8.g05
ss9.g05.k05<- ss9.g05

ss1.g1.k05 <- ss1.g1
ss2.g1.k05 <- ss2.g1
ss3.g1.k05 <- ss3.g1
ss5.g1.k05 <- ss5.g1
ss7.g1.k05 <- ss7.g1
ss8.g1.k05 <- ss8.g1
ss9.g1.k05 <- ss9.g1

ss1.g2.k05 <- ss1.g2
ss2.g2.k05 <- ss2.g2
ss3.g2.k05 <- ss3.g2
ss5.g2.k05 <- ss5.g2
ss7.g2.k05<- ss7.g2
ss8.g2.k05 <- ss8.g2
ss9.g2.k05 <- ss9.g2


#-------------------------------------------------


colors.high <- colorRampPalette(c("black", "red"))(4)
colors.low <- colorRampPalette(c("blue", "black"))(4)

colors <- c(colors.low, colors.high[-1])

## MSE
MSE.k1 <- c(ss1.k1$MSE, ss2.k1$MSE, ss3.k1$MSE, ss5.k1$MSE, ss7.k1$MSE, ss8.k1$MSE, ss9.k1$MSE,
            ss1.g05.k1$MSE, ss2.g05.k1$MSE, ss3.g05.k1$MSE, ss5.g05.k1$MSE, ss7.g05.k1$MSE, ss8.g05.k1$MSE, ss9.g05.k1$MSE,
            ss1.g1.k1$MSE, ss2.g1.k1$MSE, ss3.g1.k1$MSE, ss5.g1.k1$MSE, ss7.g1.k1$MSE, ss8.g1.k1$MSE, ss9.g1.k1$MSE,
            ss1.g2.k1$MSE, ss2.g2.k1$MSE, ss3.g2.k1$MSE, ss5.g2.k1$MSE, ss7.g2.k1$MSE, ss8.g2.k1$MSE, ss9.g2.k1$MSE)

MSE.k05 <- c(ss1.k05$MSE, ss2.k05$MSE, ss3.k05$MSE, ss5.k05$MSE, ss7.k05$MSE, ss8.k05$MSE, ss9.k05$MSE,
             ss1.g05.k05$MSE, ss2.g05.k05$MSE, ss3.g05.k05$MSE, ss5.g05.k05$MSE, ss7.g05.k05$MSE, ss8.g05.k05$MSE, ss9.g05.k05$MSE,
             ss1.g1.k05$MSE, ss2.g1.k05$MSE, ss3.g1.k05$MSE, ss5.g1.k05$MSE, ss7.g1.k05$MSE, ss8.g1.k05$MSE, ss9.g1.k05$MSE,
             ss1.g2.k05$MSE, ss2.g2.k05$MSE, ss3.g2.k05$MSE, ss5.g2.k05$MSE, ss7.g2.k05$MSE, ss8.g2.k05$MSE, ss9.g2.k05$MSE)

MSE.k4 <- c(ss1.k4$MSE, ss2.k4$MSE, ss3.k4$MSE, ss5.k4$MSE, ss7.k4$MSE, ss8.k4$MSE, ss9.k4$MSE,
            ss1.g05.k4$MSE, ss2.g05.k4$MSE, ss3.g05.k4$MSE, ss5.g05.k4$MSE, ss7.g05.k4$MSE, ss8.g05.k4$MSE, ss9.g05.k4$MSE,
            ss1.g1.k4$MSE, ss2.g1.k4$MSE, ss3.g1.k4$MSE, ss5.g1.k4$MSE, ss7.g1.k4$MSE, ss8.g1.k4$MSE, ss9.g1.k4$MSE,
            ss1.g2.k4$MSE, ss2.g2.k4$MSE, ss3.g2.k4$MSE, ss5.g2.k4$MSE, ss7.g2.k4$MSE, ss8.g2.k4$MSE, ss9.g2.k4$MSE)


df.mse <- data.frame(MSE = c(MSE.k05, MSE.k1, MSE.k4),
                     Level = factor(rep(rep(rep(c("0.1","0.2","0.3","0.5", "0.7", "0.8", "0.9"), each=500),4),3)),
                     Het = factor(rep(rep(c("0", "0.5" ,"1", "2"), each=500*7),3)),
                     kappa = factor(rep(c("0.5", "1", "4"), each=length(MSE.k1))))



appender <- function(string) {
  TeX(paste("$\\gamma = $", string))
}


dev.new()
ggplot(df.mse, aes(x=Level, y=MSE, color=Level, lty = kappa)) +
  geom_boxplot() +
  facet_grid(~Het, labeller = as_labeller(appender,
                                          default = label_parsed)) +
  scale_y_log10() +
  scale_color_manual(values = colors) +
  guides(color = "none") +
  scale_linetype_manual(values= c(3,1,2), name = expression(kappa))+
  theme( legend.title=element_text(size = rel(1.8)), legend.position = "bottom",
         plot.title = element_text(size = rel(2), face = "bold"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.title.y = element_text(size = rel(1.8)),
         axis.text.x = element_text(size = rel(1.5)),
         axis.text.y = element_text(size = rel(1.8)),
         legend.text = element_text(size = rel(1.8)), #
         strip.text.x = element_text(size = rel(1.5)),
         strip.text.y = element_text(size = rel(1.5))
  ) +
  xlab(expression(tau))+
  ylab(TeX("Log-scale of $MSE(\\tau,\\gamma)$"))
dev.off()


