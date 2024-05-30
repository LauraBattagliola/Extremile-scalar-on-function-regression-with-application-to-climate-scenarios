library(ggplot2)
library(latex2exp)
theme_set(theme_bw())
####################################################################################
################################ Plot MSE ##########################################
####################################################################################
#### kappa=1

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



colors.high <- colorRampPalette(c("black", "red"))(4)
colors.low <- colorRampPalette(c("blue", "black"))(4)

colors <- c(colors.low, colors.high[-1])

## MSE
MSE <- c(ss1$MSE, ss2$MSE, ss3$MSE, ss5$MSE, ss7$MSE, ss8$MSE, ss9$MSE,
         ss1.g05$MSE, ss2.g05$MSE, ss3.g05$MSE, ss5.g05$MSE, ss7.g05$MSE, ss8.g05$MSE, ss9.g05$MSE,
         ss1.g1$MSE, ss2.g1$MSE, ss3.g1$MSE, ss5.g1$MSE, ss7.g1$MSE, ss8.g1$MSE, ss9.g1$MSE,
         ss1.g2$MSE, ss2.g2$MSE, ss3.g2$MSE, ss5.g2$MSE, ss7.g2$MSE, ss8.g2$MSE, ss9.g2$MSE)

df.mse <- data.frame(MSE = MSE,
                     Level = factor(rep(rep(c("0.1","0.2","0.3","0.5", "0.7", "0.8", "0.9"), each=500),4)),
                     Het = factor(rep(c("0", "0.5" ,"1", "2"), each=500*7)))



appender <- function(string) {
  TeX(paste("$\\gamma = $", string))
}


dev.new()
ggplot(df.mse, aes(x=Level, y=MSE, color=Level)) +
  geom_boxplot() +
  facet_grid(~Het, labeller = as_labeller(appender,
                                          default = label_parsed)) +
  scale_y_log10() +
  scale_color_manual(values = colors) +
  theme( legend.title=element_blank(), legend.position = "none",
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

