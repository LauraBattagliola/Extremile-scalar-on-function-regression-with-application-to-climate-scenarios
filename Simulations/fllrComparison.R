################################################################################
########################## Comparison with fllr ################################
################################################################################

load("/Code/Simulations/Data/kappa1/ss_g0_tau5.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g05_tau5.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g1_tau5.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g2_tau5.Rdata")

load("/Code/Simulations/Data/kappa1/ss_g0_mean.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g05_mean.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g1_mean.Rdata")
load("/Code/Simulations/Data/kappa1/ss_g2_mean.Rdata")


mean(abs(ss5$`Estimated extremiles` - ss.mean))
mean(abs(ss5.g05$`Estimated extremiles` - ss.mean.g05))
mean(abs(ss5.g1$`Estimated extremiles` - ss.mean.g1))
mean(abs(ss5.g2$`Estimated extremiles` - ss.mean.g2))
