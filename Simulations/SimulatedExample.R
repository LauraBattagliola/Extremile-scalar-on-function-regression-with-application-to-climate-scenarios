###################################################################################
############################ Plots simulation study ###############################
###################################################################################
library(ExtrFunReg)
library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)
get_legend <- function(my_plot) {
  tmp <- ggplotGrob(my_plot)
  leg <- gtable_filter(tmp, "guide-box")
  return(leg)
}

theme_set(theme_bw())
### Example of curves
B <- 500
my_seed=12334
set.seed(my_seed)
my_seeds <- round(runif(B,min=1, max=10^6), digits=0)
dat <- simData(n=200, tau=0.5, S=100, gamma=0, myseed=my_seeds[1], sd.eps=0.25 )
dat9 <- simData(n=200, tau=0.9, S=100, gamma=0, myseed=my_seeds[1], sd.eps=0.25 )
dat1 <- simData(n=200, tau=0.1, S=100, gamma=0, myseed=my_seeds[1], sd.eps=0.25 )

set.seed(88758)
highlight_curves <- sample(1:ncol(dat[[2]]$X), 3)
data_long$curve_type <- ifelse(data_long$X %in% highlight_curves, "highlighted", "normal")
data_long <- data_long %>% arrange(curve_type)


p.X<- ggplot(data_long[which(data_long$curve_type == "normal"), ], aes(x = s, y = value, group = X)) +
  geom_line(colour = "grey", alpha = 0.5, size = 1) +
  geom_line(data = data_long[which(data_long$curve_type == "highlighted"), ], aes(x = s, y = value, group = X), colour = "chocolate4", alpha = 1, size = 1) +  # Draw highlighted curves
  labs(title = "Simuated sample example", x = "s", y = expression(X(s))) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(1.5), face = "bold"),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.1)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))


### Example of true extremiles for gamma=0 (homoskedastic)
dat9 <- simData(n=200, tau=0.9, S=100, gamma=0, myseed=my_seeds[1], sd.eps=0.25 )
dat1 <- simData(n=200, tau=0.1, S=100, gamma=0, myseed=my_seeds[1], sd.eps=0.25 )

dat_combined <- bind_rows(
  dat[[2]] %>% mutate(tau = '0.5'),
  dat1[[2]] %>% mutate(tau = '0.1'),
  dat9[[2]] %>% mutate(tau = '0.9')
)



p.xi.gamma0 <-ggplot(dat_combined, aes(x = Y, y = xi, color = tau)) +
  geom_point(pch=1, size=3, stat = "identity", stroke=1) +
  scale_color_manual(values = c("0.5" = "black", "0.1" = "blue", "0.9" = "red")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(color = expression(tau),
       y = expression(xi[tau]),
       title = "True extremiles - homoskedastic model") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(1.5), face = "bold"),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.1)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

### Example of true extremiles for gamma=1 (heteroskedastic)
dat <- simData(n=200, tau=0.5, S=100, gamma=1, myseed=my_seeds[1], sd.eps=0.25 )
dat9 <- simData(n=200, tau=0.9, S=100, gamma=1, myseed=my_seeds[1], sd.eps=0.25 )
dat1 <- simData(n=200, tau=0.1, S=100, gamma=1, myseed=my_seeds[1], sd.eps=0.25 )

dat_combined1 <- bind_rows(
  dat[[2]] %>% mutate(tau = '0.5'),
  dat1[[2]] %>% mutate(tau = '0.1'),
  dat9[[2]] %>% mutate(tau = '0.9')
)


p.xi.gamma1 <- ggplot(dat_combined1, aes(x = Y, y = xi, color = tau)) +
  geom_point(pch=1, size=3, stat = "identity", stroke=1) +
  scale_color_manual(values = c("0.5" = "black", "0.1" = "blue", "0.9" = "red")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(color = expression(tau),
       y = expression(xi[tau]),
       title = "True extremiles - heteroskedastic model") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(1.5), face = "bold"),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.1)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))



legend <- get_legend(p.xi.gamma1)

dev.new()
grid.arrange(
  arrangeGrob(p.X,
              p.xi.gamma0 + theme(legend.position = "none"),
              p.xi.gamma1 + theme(legend.position = "none"), nrow=1),
  legend, ncol = 1, heights = c(10, 1))
dev.off()
