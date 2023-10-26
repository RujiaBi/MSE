library(ggplot2)
library(ggforce)
library(matrixStats)
library(ggdist)
library(ggpubr)
library(cowplot)


res = inla.collect.results("inla.model")
res_tac <- res$summary.random$tac[,c(1:2)]


## Intercept+f(Tac_change) on log(catch/TAC)
times = 1000
samps = inla.posterior.sample(times,res)

fun = function(...) {
  exp(mu + tac)-0.1
}
f = inla.posterior.sample.eval(fun, samps)

res_tac$tac_change <- exp(res_tac$ID)-1
res_tac$Mean <- rowMeans(f)
res_tac$Median <- rowMedians(f)
res_tac$p025 <- rowQuantiles(f, probs = 0.025)
res_tac$p975 <- rowQuantiles(f, probs = 0.975)

# log(TAC change+1)
g <- ggplot(data=res_tac) +
  geom_smooth(aes(x=ID, y=Median),method = 'loess', span =  0.4, se=F) +
  geom_smooth(aes(x=ID, y=p025),method = 'loess', span = 0.4, se=F, linetype = "dashed") +
  geom_smooth(aes(x=ID, y=p975),method = 'loess', span = 0.4, se=F, linetype = "dashed")
g1 <- ggplot_build(g)

df1 <- data.frame(x = g1$data[[1]]$x,
                  y = g1$data[[1]]$y,
                  ymin = g1$data[[2]]$y,
                  ymax = g1$data[[3]]$y) 

# TAC change
g <- ggplot(data=res_tac) +
  geom_smooth(aes(x=tac_change, y=Median),method = 'loess', span =  0.4, se=F) +
  geom_smooth(aes(x=tac_change, y=p025),method = 'loess', span = 0.4, se=F, linetype = "dashed") +
  geom_smooth(aes(x=tac_change, y=p975),method = 'loess', span = 0.4, se=F, linetype = "dashed")
g2 <- ggplot_build(g)

df2 <- data.frame(x = g2$data[[1]]$x,
                  y = g2$data[[1]]$y,
                  ymin = g2$data[[2]]$y,
                  ymax = g2$data[[3]]$y) 



p1 <- ggplot(df1) +
  geom_line(aes(x=x, y=y), size=1) + 
  geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), alpha=.25, linetype = 0) + 
  #geom_line(aes(x=ID, y=Median), size=1) + 
  #geom_ribbon(aes(x=ID, ymin=p025, ymax=p975), alpha=.25, linetype = 0) + 
  labs(x="log(TAC change+1)", y="Predicted Catch/TAC") +
  geom_segment(y=-0.092, yend=-0.092, x=-4, xend=4, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=0, yend=2, x=-4.7, xend=-4.7, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(aes(x=-4.3,xend=4.04, y=1, yend=1), linetype = "dotted", col="blue", linewidth=1) +
  scale_x_continuous(breaks=seq(-4, 4, 2), limits=c(-4.3,4.04)) +
  scale_y_continuous(breaks=seq(0, 2, 0.5), limits=c(0, 2)) +
  theme_classic(base_size=12) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y   = element_text(size=24),
        axis.text.x   = element_text(size=24),
        axis.title.y  = element_text(size=26),
        axis.title.x  = element_text(size=26),
        plot.margin = unit(c(1,1,1,1), "lines"),
        panel.spacing = unit(1, "lines"),
        axis.ticks.length.x = unit(7, "pt"),
        axis.ticks.length.y = unit(7, "pt"),
        axis.ticks=element_line(size=0.65)) 

p2 <- ggplot(df2) +
  geom_line(aes(x=x, y=y), size=1) + 
  geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), alpha=.25, linetype = 0) + 
  labs(x="TAC change", y="Predicted Catch/TAC") +
  geom_segment(y=-0.092, yend=-0.092, x=-1, xend=56, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=0, yend=2, x=-3.7, xend=-3.7, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(aes(x=-1,xend=56, y=1, yend=1), linetype = "dotted", col="blue", linewidth=1) +
  scale_x_continuous(breaks=c(-1, 7, 15, 23, 31, 39, 47, 56), limits=c(-1,56)) +
  scale_y_continuous(breaks=seq(0, 2, 0.5), limits=c(0, 2)) +
  theme_classic(base_size=12) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y   = element_text(size=24),
        axis.text.x   = element_text(size=24),
        axis.title.y  = element_text(size=26),
        axis.title.x  = element_text(size=26),
        plot.margin = unit(c(1,1,1,1), "lines"),
        panel.spacing = unit(1, "lines"),
        axis.ticks.length.x = unit(7, "pt"),
        axis.ticks.length.y = unit(7, "pt"),
        axis.ticks=element_line(size=0.65)) 

## Put all figures together ##
p <- ggdraw() +
  # first row
  draw_plot(p1, x = 0, y = .5, width = 1, height = .5) +

  # second row
  draw_plot(p2, x = 0, y = 0, width = 1, height = .5) +
  
  
  draw_plot_label(label = c("A", "B"), size = 30,
                  x = c(.85, .85), 
                  y = c(1, .5))


jpeg("Intercept+tac.jpeg", height=10, width=6.5, units='in', res=400)
p
dev.off()

