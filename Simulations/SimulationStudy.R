library(ExtrFunReg)
library(parallelDist)
library(fllr)

################################################################################
############################ Simulation study ##################################
################################################################################


B <- 500 ## number of iterations
n <- 200 ## number of observations
S <- 100 ## number of regular grid points

## Extremile levels
tau1 <- 0.1
tau2 <- 0.2
tau3 <- 0.3
tau5 <- 0.5
tau7 <- 0.7
tau8 <- 0.8
tau9 <- 0.9

## Heteroskedasticity parameter
gamma0 <- 0
gamma05 <- 0.5
gamma1 <- 1
gamma4 <- 4
gamma2 <- 2

## Grid length for local bandwidth of conditional CDF estimator
n.hF <- 10
#----------------------------------------------------------------------------------------------------------------
## kappa=1
kappa <- 1
## Same generative and estimating tau - gamma=0
ss5<- SimStudy(n, S, gamma0, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=12334, kappa=kappa, n.hF = n.hF)
ss1<- SimStudy(n, S, gamma0, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss2<- SimStudy(n, S, gamma0, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss3<- SimStudy(n, S, gamma0, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss7<- SimStudy(n, S, gamma0, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss8<- SimStudy(n, S, gamma0, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss9<- SimStudy(n, S, gamma0, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
## Comparison with fllr package (mean local linear regression)
ss.mean <- SimStudy.fllr(n, S, gamma0, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=12334)

setwd("/Code/Simulations/Data/kappa1")
save(ss1, file="ss_g0_tau1.Rdata")
save(ss2, file="ss_g0_tau2.Rdata")
save(ss3, file="ss_g0_tau3.Rdata")
save(ss5, file="ss_g0_tau5.Rdata")
save(ss7, file="ss_g0_tau7.Rdata")
save(ss8, file="ss_g0_tau8.Rdata")
save(ss9, file="ss_g0_tau9.Rdata")
save(ss.mean, file="ss_g0_mean.Rdata")

#----------------------------------------------------------------------------------------------------------------
## Same generative and estimating tau - gamma=0.5
kappa <- 1
ss5.g05<- SimStudy(n, S, gamma05, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=56464, kappa=kappa, n.hF = n.hF)
ss1.g05<- SimStudy(n, S, gamma05, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss2.g05<- SimStudy(n, S, gamma05, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss3.g05<- SimStudy(n, S, gamma05, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss7.g05<- SimStudy(n, S, gamma05, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss8.g05<- SimStudy(n, S, gamma05, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss9.g05<- SimStudy(n, S, gamma05, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
## Comparison with fllr package (mean local linear regression)
ss.mean.g05 <- SimStudy.fllr(n, S, gamma05, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=56464)

setwd("/Code/Simulations/Data/kappa1")
save(ss1.g05, file="ss_g05_tau1.Rdata")
save(ss2.g05, file="ss_g05_tau2.Rdata")
save(ss3.g05, file="ss_g05_tau3.Rdata")
save(ss5.g05, file="ss_g05_tau5.Rdata")
save(ss7.g05, file="ss_g05_tau7.Rdata")
save(ss8.g05, file="ss_g05_tau8.Rdata")
save(ss9.g05, file="ss_g05_tau9.Rdata")
save(ss.mean.g05, file="ss_g05_mean.Rdata")

#-----------------------------------------------------------------------------
## Same generative and estimating tau - gamma=1
kappa <- 1
ss5.g1<- SimStudy(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=46335, kappa=kappa, n.hF = n.hF)
ss1.g1<- SimStudy(n, S, gamma1, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss2.g1<- SimStudy(n, S, gamma1, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss3.g1<- SimStudy(n, S, gamma1, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss7.g1<- SimStudy(n, S, gamma1, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss8.g1<- SimStudy(n, S, gamma1, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss9.g1<- SimStudy(n, S, gamma1, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
## Comparison with fllr package (mean local linear regression)
ss.mean.g1 <- SimStudy.fllr(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=46335)

setwd("/Code/Simulations/Data/kappa1")
save(ss1.g1, file="ss_g1_tau1.Rdata")
save(ss2.g1, file="ss_g1_tau2.Rdata")
save(ss3.g1, file="ss_g1_tau3.Rdata")
save(ss5.g1, file="ss_g1_tau5.Rdata")
save(ss7.g1, file="ss_g1_tau7.Rdata")
save(ss8.g1, file="ss_g1_tau8.Rdata")
save(ss9.g1, file="ss_g1_tau9.Rdata")
save(ss.mean.g1, file="ss_g1_mean.Rdata")

#---------------------------------------------------------------
## Same generative and estimating tau - gamma=2
kappa <- 1
ss5.g2<- SimStudy(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=242355, kappa=kappa, n.hF = n.hF)
ss1.g2<- SimStudy(n, S, gamma1, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss2.g2<- SimStudy(n, S, gamma1, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss3.g2<- SimStudy(n, S, gamma1, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss7.g2<- SimStudy(n, S, gamma1, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss8.g2<- SimStudy(n, S, gamma1, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss9.g2<- SimStudy(n, S, gamma1, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
## Comparison with fllr package (mean local linear regression)
ss.mean.g2 <- SimStudy.fllr(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=242355)

setwd("/Code/Simulations/Data/kappa1")
save(ss1.g2, file="ss_g2_tau1.Rdata")
save(ss2.g2, file="ss_g2_tau2.Rdata")
save(ss3.g2, file="ss_g2_tau3.Rdata")
save(ss5.g2, file="ss_g2_tau5.Rdata")
save(ss7.g2, file="ss_g2_tau7.Rdata")
save(ss8.g2, file="ss_g2_tau8.Rdata")
save(ss9.g2, file="ss_g2_tau9.Rdata")
save(ss.mean.g2, file="ss_g2_mean.Rdata")
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
### kappa=4
kappa <- 4

## Same generative and estimating tau - gamma=0
ss5<- SimStudy(n, S, gamma0, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=12334, kappa=kappa, n.hF = n.hF)
ss1<- SimStudy(n, S, gamma0, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss2<- SimStudy(n, S, gamma0, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss3<- SimStudy(n, S, gamma0, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss7<- SimStudy(n, S, gamma0, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss8<- SimStudy(n, S, gamma0, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss9<- SimStudy(n, S, gamma0, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)



setwd("/Code/Simulations/Data/kappa4")
save(ss1, file="ss_g0_tau1.Rdata")
save(ss2, file="ss_g0_tau2.Rdata")
save(ss3, file="ss_g0_tau3.Rdata")
save(ss5, file="ss_g0_tau5.Rdata")
save(ss7, file="ss_g0_tau7.Rdata")
save(ss8, file="ss_g0_tau8.Rdata")
save(ss9, file="ss_g0_tau9.Rdata")


#----------------------------------------------------------------------------------------------------------------
## Same generative and estimating tau - gamma=0.5
kappa <- 4
ss5.g05<- SimStudy(n, S, gamma05, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=56464, kappa=kappa, n.hF = n.hF)
ss1.g05<- SimStudy(n, S, gamma05, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss2.g05<- SimStudy(n, S, gamma05, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss3.g05<- SimStudy(n, S, gamma05, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss7.g05<- SimStudy(n, S, gamma05, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss8.g05<- SimStudy(n, S, gamma05, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss9.g05<- SimStudy(n, S, gamma05, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)



setwd("/Code/Simulations/Data/kappa4")
save(ss1.g05, file="ss_g05_tau1.Rdata")
save(ss2.g05, file="ss_g05_tau2.Rdata")
save(ss3.g05, file="ss_g05_tau3.Rdata")
save(ss5.g05, file="ss_g05_tau5.Rdata")
save(ss7.g05, file="ss_g05_tau7.Rdata")
save(ss8.g05, file="ss_g05_tau8.Rdata")
save(ss9.g05, file="ss_g05_tau9.Rdata")


#-----------------------------------------------------------------------------
## Same generative and estimating tau - gamma=1
kappa <- 4
ss5.g1<- SimStudy(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=46335, kappa=kappa, n.hF = n.hF)
ss1.g1<- SimStudy(n, S, gamma1, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss2.g1<- SimStudy(n, S, gamma1, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss3.g1<- SimStudy(n, S, gamma1, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss7.g1<- SimStudy(n, S, gamma1, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss8.g1<- SimStudy(n, S, gamma1, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss9.g1<- SimStudy(n, S, gamma1, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)



setwd("/Code/Simulations/Data/kappa4")
save(ss1.g1, file="ss_g1_tau1.Rdata")
save(ss2.g1, file="ss_g1_tau2.Rdata")
save(ss3.g1, file="ss_g1_tau3.Rdata")
save(ss5.g1, file="ss_g1_tau5.Rdata")
save(ss7.g1, file="ss_g1_tau7.Rdata")
save(ss8.g1, file="ss_g1_tau8.Rdata")
save(ss9.g1, file="ss_g1_tau9.Rdata")


#---------------------------------------------------------------
## Same generative and estimating tau - gamma=2
kappa <- 4
ss5.g2<- SimStudy(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=242355, kappa=kappa, n.hF = n.hF)
ss1.g2<- SimStudy(n, S, gamma1, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss2.g2<- SimStudy(n, S, gamma1, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss3.g2<- SimStudy(n, S, gamma1, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss7.g2<- SimStudy(n, S, gamma1, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss8.g2<- SimStudy(n, S, gamma1, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss9.g2<- SimStudy(n, S, gamma1, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)



setwd("/Code/Simulations/Data/kappa4")
save(ss1.g2, file="ss_g2_tau1.Rdata")
save(ss2.g2, file="ss_g2_tau2.Rdata")
save(ss3.g2, file="ss_g2_tau3.Rdata")
save(ss5.g2, file="ss_g2_tau5.Rdata")
save(ss7.g2, file="ss_g2_tau7.Rdata")
save(ss8.g2, file="ss_g2_tau8.Rdata")
save(ss9.g2, file="ss_g2_tau9.Rdata")


#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
### kappa=0.5

kappa <- 0.5
## Same generative and estimating tau - gamma=0
ss5<- SimStudy(n, S, gamma0, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=12334, kappa=kappa, n.hF = n.hF)
ss1<- SimStudy(n, S, gamma0, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss2<- SimStudy(n, S, gamma0, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss3<- SimStudy(n, S, gamma0, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss7<- SimStudy(n, S, gamma0, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss8<- SimStudy(n, S, gamma0, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)
ss9<- SimStudy(n, S, gamma0, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=12334, hFMat = ss5[[4]], kappa=kappa, n.hF = n.hF)

ss.mean <- SimStudy.fllr(n, S, gamma0, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=12334)

setwd("/Code/Simulations/Data/kappa05")
save(ss1, file="ss_g0_tau1.Rdata")
save(ss2, file="ss_g0_tau2.Rdata")
save(ss3, file="ss_g0_tau3.Rdata")
save(ss5, file="ss_g0_tau5.Rdata")
save(ss7, file="ss_g0_tau7.Rdata")
save(ss8, file="ss_g0_tau8.Rdata")
save(ss9, file="ss_g0_tau9.Rdata")
save(ss.mean, file="ss_g0_mean.Rdata")

#----------------------------------------------------------------------------------------------------------------
## Same generative and estimating tau - gamma=0.5
kappa <- 0.5
ss5.g05<- SimStudy(n, S, gamma05, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=56464, kappa=kappa, n.hF = n.hF)
ss1.g05<- SimStudy(n, S, gamma05, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss2.g05<- SimStudy(n, S, gamma05, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss3.g05<- SimStudy(n, S, gamma05, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss7.g05<- SimStudy(n, S, gamma05, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss8.g05<- SimStudy(n, S, gamma05, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)
ss9.g05<- SimStudy(n, S, gamma05, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=56464, hFMat = ss5.g05[[4]], kappa=kappa, n.hF = n.hF)

ss.mean.g05 <- SimStudy.fllr(n, S, gamma05, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=56464)

setwd("/Code/Simulations/Data/kappa05")
save(ss1.g05, file="ss_g05_tau1.Rdata")
save(ss2.g05, file="ss_g05_tau2.Rdata")
save(ss3.g05, file="ss_g05_tau3.Rdata")
save(ss5.g05, file="ss_g05_tau5.Rdata")
save(ss7.g05, file="ss_g05_tau7.Rdata")
save(ss8.g05, file="ss_g05_tau8.Rdata")
save(ss9.g05, file="ss_g05_tau9.Rdata")
save(ss.mean.g05, file="ss_g05_mean.Rdata")

#-----------------------------------------------------------------------------
## Same generative and estimating tau - gamma=1
kappa <- 0.5
ss5.g1<- SimStudy(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=46335, kappa=kappa, n.hF = n.hF)
ss1.g1<- SimStudy(n, S, gamma1, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss2.g1<- SimStudy(n, S, gamma1, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss3.g1<- SimStudy(n, S, gamma1, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss7.g1<- SimStudy(n, S, gamma1, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss8.g1<- SimStudy(n, S, gamma1, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)
ss9.g1<- SimStudy(n, S, gamma1, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=46335, hFMat = ss5.g1[[4]], kappa=kappa, n.hF = n.hF)

ss.mean.g1 <- SimStudy.fllr(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=46335)

setwd("/Code/Simulations/Data/kappa05")
save(ss1.g1, file="ss_g1_tau1.Rdata")
save(ss2.g1, file="ss_g1_tau2.Rdata")
save(ss3.g1, file="ss_g1_tau3.Rdata")
save(ss5.g1, file="ss_g1_tau5.Rdata")
save(ss7.g1, file="ss_g1_tau7.Rdata")
save(ss8.g1, file="ss_g1_tau8.Rdata")
save(ss9.g1, file="ss_g1_tau9.Rdata")
save(ss.mean.g1, file="ss_g1_mean.Rdata")

#---------------------------------------------------------------
## Same generative and estimating tau - gamma=2
kappa <- 0.5
ss5.g2<- SimStudy(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=242355, kappa=kappa, n.hF = n.hF)
ss1.g2<- SimStudy(n, S, gamma1, tau.gen=tau1, sd.eps = 0.25, tau.est=tau1, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss2.g2<- SimStudy(n, S, gamma1, tau.gen=tau2, sd.eps = 0.25, tau.est=tau2, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss3.g2<- SimStudy(n, S, gamma1, tau.gen=tau3, sd.eps = 0.25, tau.est=tau3, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss7.g2<- SimStudy(n, S, gamma1, tau.gen=tau7, sd.eps = 0.25, tau.est=tau7, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss8.g2<- SimStudy(n, S, gamma1, tau.gen=tau8, sd.eps = 0.25, tau.est=tau8, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)
ss9.g2<- SimStudy(n, S, gamma1, tau.gen=tau9, sd.eps = 0.25, tau.est=tau9, percent=0.95, B, my_seed=242355, hFMat = ss5.g2[[4]], kappa=kappa, n.hF = n.hF)

ss.mean.g2 <- SimStudy.fllr(n, S, gamma1, tau.gen=tau5, sd.eps = 0.25, tau.est=tau5, percent=0.95, B, my_seed=242355)

setwd("/Code/Simulations/Data/kappa05")
save(ss1.g2, file="ss_g2_tau1.Rdata")
save(ss2.g2, file="ss_g2_tau2.Rdata")
save(ss3.g2, file="ss_g2_tau3.Rdata")
save(ss5.g2, file="ss_g2_tau5.Rdata")
save(ss7.g2, file="ss_g2_tau7.Rdata")
save(ss8.g2, file="ss_g2_tau8.Rdata")
save(ss9.g2, file="ss_g2_tau9.Rdata")
save(ss.mean.g2, file="ss_g2_mean.Rdata")
