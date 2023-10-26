library(ggplot2)
library(ggforce)
library(ggdist)
library(ggpubr)
library(cowplot)
library(dplyr)

## A: catch/tac violin plot ##
p1 <- ggplot(dd, aes(catch_by_TAC)) +
  geom_histogram(bins=80) +
  labs(x="Catch/TAC", y="Frequency") +
  geom_segment(y=-69, yend=-69, x=0, xend=8, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=0, yend=1500, x=-0.43, xend=-0.43, lwd=0.8, colour="grey30", lineend="square") +
  #scale_x_continuous(breaks=seq(0, 8, 2), limits=c(0,8.1)) +
  scale_y_continuous(breaks=seq(0, 1500, 500), limits=c(0, 1500)) +
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


## Log(catch/TAC+0.1) histogram
dat$log_catch_by_TAC <- log(dat$catch_by_TAC+0.1)

p8 <- ggplot(dat, aes(log_catch_by_TAC)) +
  geom_histogram(bins=80) +
  labs(x="log(Catch/TAC+0.1)", y="Frequency") +
  geom_segment(y=-45.6, yend=-45.6, x=-2.5, xend=2.5, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=0, yend=1000, x=-2.74, xend=-2.74, lwd=0.8, colour="grey30", lineend="square") +
  scale_x_continuous(breaks=seq(-2.5, 2.5, 2.5), limits=c(-2.5,2.5)) +
  scale_y_continuous(breaks=seq(0, 1000, 250), limits=c(0, 1000)) +
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

## TAC_change histogram
p9 <- ggplot(dat, aes(tac_change)) +
  geom_histogram(bins=80) +
  labs(x="TAC change", y="Frequency") +
  geom_segment(y=-290, yend=-290, x=0, xend=50, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=0, yend=6000, x=-3.8, xend=-3.8, lwd=0.8, colour="grey30", lineend="square") +
  scale_x_continuous(breaks=seq(0, 50, 10)) +
  scale_y_continuous(breaks=seq(0, 6000, 2000), limits=c(0, 6100)) +
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


## log(TAC_change+1) histogram
dat$log_tac_change <- log(dat$tac_change+1)

p10 <- ggplot(dat, aes(log_tac_change)) +
  geom_histogram(bins=80) +
  labs(x="log(TAC change+1)", y="Frequency") +
  geom_segment(y=-190, yend=-190, x=-5, xend=5, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=0, yend=4000, x=-5.5, xend=-5.5, lwd=0.8, colour="grey30", lineend="square") +
  scale_x_continuous(breaks=seq(-5, 5, 2.5), limits=c(-5,5)) +
  scale_y_continuous(breaks=seq(0, 4000, 1000), limits=c(0, 4000)) +
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


## B: region-catch/tac violin plot ##
dat[dat$council=="Canada east",]$council <- "Canada East"
dat[dat$council=="Canada west",]$council <- "Canada West"
dat[dat$council=="AOHS",]$council <- "AOHSs"
dat[dat$council=="SOs",]$council <- "SOHSs"
dat[dat$council=="POHS",]$council <- "POHSs"
dat[dat$council=="Europe-Med.BlackS",]$council <- "Europe-MBS"
dat[dat$council=="Russia east",]$council <- "Russia East"
dat[dat$council=="US non-federal",]$council <- "US Non-Federal"
dat$council <- factor(dat$council, levels=c("ASMFC", "NEFMC", "MAFMC", "SAFMC", "GMFMC", 
                                            "NPFMC", "PFMC", "US Non-Federal", "Canada East", 
                                            "Canada West", "South America", "Europe-ICES-EU", 
                                            "Europe-ICES-nonEU", "Europe-MBS", "Australia", 
                                            "New Zealand", "Japan", "Russia East", "South Africa", 
                                            "West Africa", "AOHSs", "IPHC", "POHSs", "SOHSs"))

p2 <- ggplot(dat, aes(council, log_catch_by_TAC)) + 
  #geom_point(size = .8, alpha = .2, position = position_jitter(seed = 1, width = .1)) +
  #ggdist::stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.2, point_colour = NA) +
  #geom_point(shape = "|", size = 1, alpha = .2) +
  geom_boxplot() +
  stat_summary(fun.y = median, fun.ymax = length, geom = "text", aes(label = ..ymax..), vjust = -1, col="blue") +
  geom_segment(x=0.4, xend=0.4, y=-2.5, yend=2.5, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=-2.75, yend=-2.75, x=1, xend=24, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(aes(x=1,xend=24, y=log(1+0.1), yend=log(1+0.1)), linetype = "dotted", col="red", linewidth=1.6) +
  scale_y_continuous(breaks=seq(-2.5, 2.5, 1), limits=c(-2.5, 2.5)) +
  labs(x="Region", y="log(Catch/TAC+0.1)") +
  theme_classic(base_size=12) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y   = element_text(size=24),
        axis.text.x   = element_text(size=24, angle = 50, vjust = 1, hjust=1),
        axis.title.y  = element_text(size=26),
        axis.title.x  = element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines"),
        panel.spacing = unit(1, "lines"),
        axis.ticks.length.x = unit(7, "pt"),
        axis.ticks.length.y = unit(7, "pt"),
        axis.ticks=element_line(size=0.65)) 


## C: Habitat-catch/tac violin plot ##
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
dat$DemersPelag <- firstup(dat$DemersPelag)

p3 <- ggplot(dat, aes(DemersPelag, log_catch_by_TAC)) + 
  #geom_point(size = .8, alpha = .2, position = position_jitter(seed = 1, width = .1)) +
  #ggdist::stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.2, point_colour = NA) +
  #geom_point(shape = "|", size = 1, alpha = .2) +
  #geom_text(data = dat_summary, aes(DemersPelag, 2.5, label = n)) +
  geom_boxplot() +
  stat_summary(fun.y = median, fun.ymax = length, geom = "text", aes(label = ..ymax..), vjust = -1, col="blue") +
  geom_segment(x=0.4, xend=0.4, y=-2.5, yend=2.5, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=-2.75, yend=-2.75, x=1, xend=9, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(aes(x=1,xend=9, y=log(1+0.1), yend=log(1+0.1)), linetype = "dotted", col="red", linewidth=1.6) +
  scale_y_continuous(breaks=seq(-2.5, 2.5, 1), limits=c(-2.5, 2.5)) +
  labs(x="Habitat", y="log(Catch/TAC+0.1)") +
  theme_classic(base_size=12) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y   = element_text(size=24),
        axis.text.x   = element_text(size=24, angle = 50, vjust = 1, hjust=1),
        axis.title.y  = element_text(size=26),
        axis.title.x  = element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines"),
        panel.spacing = unit(1, "lines"),
        axis.ticks.length.x = unit(7, "pt"),
        axis.ticks.length.y = unit(7, "pt"),
        axis.ticks=element_line(size=0.65)) 


## D: scatter plot of TAC_change and Catch/TAC ##
dat$log_tac_change <- log(dat$tac_change+1)

p4 <- ggplot(dat, aes(x=log_tac_change, y=log_catch_by_TAC)) +
  geom_point(size=3, shape=21) +
  labs(x="log(TAC change+1)", y="log(Catch/TAC+0.1)") +
  geom_segment(y=-2.75, yend=-2.75, x=-5, xend=5, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=-2.5, yend=2.5, x=-5.49, xend=-5.49, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(aes(x=-5,xend=5, y=log(1+0.1), yend=log(1+0.1)), linetype = "dotted", col="red", linewidth=1.6) +
  geom_segment(aes(x=log(0+1),xend=log(0+1), y=-2.5, yend=2.5), linetype = "dotted", col="blue", linewidth=1.6) +
  scale_x_continuous(breaks=seq(-5, 5, 2.5), limits=c(-5,5)) +
  scale_y_continuous(breaks=seq(-2.5, 2.5, 1), limits=c(-2.5, 2.5)) +
  geom_text(data = data.frame(x=4.28, y=log(1+0.1)+0.3, label="Catch/TAC=1"), aes(x=x, y=y, label=label), size=6, col="red") +
  geom_text(data = data.frame(x=log(0+1), y=2.5, label="TAC change=0"), aes(x=x, y=y, label=label), size=6, col="blue") +
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

## E: Year-catch/tac violin plot ##
dat$year <- as.factor(dat$year)

p5 <- ggplot(dat, aes(year, log_catch_by_TAC)) + 
  geom_boxplot() +
  stat_summary(fun.y = median, fun.ymax = length, geom = "text", aes(label = ..ymax..), vjust = -1, colour="blue") +
  geom_segment(x=0.45, xend=0.45, y=-2.5, yend=2.5, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=-2.75, yend=-2.75, x=1, xend=59, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(aes(x=1,xend=59, y=log(1+0.1), yend=log(1+0.1)), linetype = "dotted", col="red", linewidth=1.6) +
  scale_y_continuous(breaks=seq(-2.5, 2.5, 1), limits=c(-2.5, 2.5)) +
  scale_x_discrete(breaks=c(1964, 1973, 1983, 1993, 2003, 2013, 2022)) +
  labs(x="Year", y="log(Catch/TAC+0.1)") +
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
        axis.title.x  = element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines"),
        panel.spacing = unit(1, "lines"),
        axis.ticks.length.x = unit(7, "pt"),
        axis.ticks.length.y = unit(7, "pt"),
        axis.ticks=element_line(size=0.65)) 


## F & G: Year patterns ##
# region key
region_key <- data.frame(matrix(NA, nrow=24, ncol=2))
colnames(region_key) <- c("council_code", "Region")
region_key$council_code <- c(1:24)
region_key$Region <- c("AOHSs", "ASMFC", "Australia", "Canada East", "Canada West", "Europe-ICES-EU", "Europe-ICES-nonEU", 
                       "Europe-MBS", "GMFMC", "IPHC", "Japan", "MAFMC", "NEFMC", "New Zealand", "NPFMC", "PFMC", "POHSs", 
                       "Russia East", "SAFMC", "SOHSs", "South Africa",  "South America", "US Non-Federal", "West Africa")

outputdir <- "~/Desktop/MSE/data_preparation/Output"
dat <- read.csv(paste(outputdir, "1_dat_for_model.csv", sep="/"), as.is=T)
dat$country <- dat$region
table(dat$country)
dat[dat$region=="Canada east" | dat$region=="Canada west",]$country <- "Canada"
dat[dat$region=="Europe-ICES-EU" | dat$region=="Europe-ICES-nonEU" | dat$region=="Europe-Med.BlackS",]$country <- "Europe"
dat[dat$region=="US Alaska" | dat$region=="US northeast" | dat$region=="US southeast" | dat$region=="US west",]$country <- "US"
dat$country_code <- as.numeric(as.factor(dat$country))

# Organize data for plotting
dd <- unique(dat[,c(10,11,3,4)])
dd <- merge(dd, region_key, by="council_code", all.x=TRUE)
dd$Region <- factor(dd$Region, levels=c("ASMFC", "NEFMC", "MAFMC", "SAFMC", "GMFMC", 
                                        "NPFMC", "PFMC", "US Non-Federal", "Canada East", 
                                        "Canada West", "South America", "Europe-ICES-EU", 
                                        "Europe-ICES-nonEU", "Europe-MBS", "Australia", 
                                        "New Zealand", "Japan", "Russia East", "South Africa", 
                                        "West Africa", "AOHSs", "IPHC", "POHSs", "SOHSs"))
dd <- dd[order(dd$year),]
stock_key <- data.frame(matrix(NA, nrow=420, ncol=2))
colnames(stock_key) <- c("stock", "stock_code_new")
stock_key$stock_code_new <- c(1:420)
stock_key$stock <- unique(dd$stock)

dd <- merge(dd, stock_key, by="stock", all.x=TRUE)


# Group average
cc <- unique(dat[,c(11,4)])
cc <- merge(cc, region_key, by="council_code", all.x=TRUE)
cc$Region <- factor(cc$Region, levels=c("ASMFC", "NEFMC", "MAFMC", "SAFMC", "GMFMC", 
                                        "NPFMC", "PFMC", "US Non-Federal", "Canada East", 
                                        "Canada West", "South America", "Europe-ICES-EU", 
                                        "Europe-ICES-nonEU", "Europe-MBS", "Australia", 
                                        "New Zealand", "Japan", "Russia East", "South Africa", 
                                        "West Africa", "AOHSs", "IPHC", "POHSs", "SOHSs"))
cc$region_code <- as.numeric(cc$Region)


cbPalette2 <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
                "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
                "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D")


p6 <- ggplot(dd) +
  geom_point(aes(year, stock_code_new, colour=Region), size=1.5, shape = 19) +
  scale_colour_manual(values=cbPalette2) +
  labs(x="Year", y="Stock count") +
  geom_segment(y=-20, yend=-20, x=1964, xend=2022, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=1, yend=420, x=1961.3, xend=1961.3, lwd=0.8, colour="grey30", lineend="square") +
  scale_x_continuous(breaks=seq(1964, 2022, 29), limits=c(1964,2022)) +
  scale_y_continuous(breaks=seq(0, 420, 60), limits=c(0, 420)) +
  theme_classic(base_size=12) +
  theme_bw() + 
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y   = element_text(size=24),
    axis.text.x   = element_text(size=24),
    axis.title.y  = element_text(size=26),
    axis.title.x  = element_blank(),
    legend.title=element_text(size=24),
    legend.text=element_text(size=22),
    legend.box="vertical", 
    legend.margin=margin(-40,0,0,0),
    legend.spacing.y = unit(0.8, "lines"),
    strip.text.x = element_text(size=36),
    strip.text.y = element_text(size=36),
    strip.background = element_blank(),
    plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
    panel.spacing = unit(1, "lines"),
    axis.ticks.length.x = unit(7, "pt"),
    axis.ticks.length.y = unit(7, "pt")) +
  guides(colour = guide_legend(ncol = 2, bycol = T, override.aes = list(size=3)))


p7 <- ggplot(cc) +
  #geom_errorbarh(aes(xmax=tmax, xmin=tmin, y=region_code, colour=Region, height=0), size=1.5) +
  geom_point(aes(year, region_code, colour=Region), size=2.5, shape = 19) +
  scale_colour_manual(values=cbPalette2) +
  labs(x="Year", y="Region") +
  geom_segment(y=-1.2, yend=-1.2, x=1964, xend=2022, lwd=0.8, colour="grey30", lineend="square") +
  geom_segment(y=0, yend=25, x=1961.3, xend=1961.3, lwd=0.8, colour="grey30", lineend="square") +
  scale_x_continuous(breaks=seq(1964, 2022, 29), limits=c(1964,2022)) +
  scale_y_continuous(breaks=seq(0, 25, 5), limits=c(0, 25)) +
  theme_classic(base_size=12) +
  theme_bw() + 
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.y =  element_blank(), 
    axis.line.x =  element_blank(), 
    axis.text.y   = element_text(size=24),
    axis.text.x   = element_text(size=24),
    axis.title.y  = element_text(size=26),
    axis.title.x  = element_blank(),
    legend.title=element_text(size=24),
    legend.text=element_text(size=22),
    legend.box="vertical", 
    legend.margin=margin(-40,0,0,0),
    legend.spacing.y = unit(0.8, "lines"),
    strip.text.x = element_text(size=36),
    strip.text.y = element_text(size=36),
    strip.background = element_blank(),
    plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
    panel.spacing = unit(1, "lines"),
    axis.ticks.length.x = unit(7, "pt"),
    axis.ticks.length.y = unit(7, "pt")) +
  guides(colour = guide_legend(ncol = 2, bycol = T, override.aes = list(size=3)))

legend <- get_legend(
  # create some space to the left of the legend
  p6 + theme(legend.box.margin = margin(20,0,0,0))
)

p <- ggdraw() +
  draw_plot(p6 + theme(legend.position="none"), x = 0, y = 0, width = .5, height = 1) +
  draw_plot(p7 + theme(legend.position="none"), x = .5, y = 0, width = .5, height = 1) +
  draw_plot_label(label = c("I", "J"), size = 30,
                  x = c(0.13, 0.6), y = c(1, 1))

p6_7 <- plot_grid(p, legend, rel_widths = c(0.65, 0.35))



## Put all figures together ##
p <- ggdraw() +
  # first row
  draw_plot(p1, x = 0, y = .84, width = .33, height = .16) +
  draw_plot(p9, x = .33, y = .84, width = .33, height = .16) +
  draw_plot(p8, x = .66, y = .84, width = .34, height = .16) +
  
  # second row
  draw_plot(p10, x = 0, y = .66, width = .33, height = .18) +
  draw_plot(p4, x = .33, y = .66, width = .66, height = .18) +
  
  # third row
  draw_plot(p3, x = 0, y = .41, width = .4, height = .25) +
  draw_plot(p2, x = .4, y = .41, width = .6, height = .25) +

  # fourth row
  draw_plot(p5, x = 0, y = .2, width = 1, height = .21) +
  
  # fifth row
  draw_plot(p6_7, x = 0, y = 0, width = 1, height = .2) +
  
  
  draw_plot_label(label = c("A", "B", "C", "D", "E", "F", "G", "H"), size = 30,
                  x = c(.27, .6, .94, .27, .94, .35, .94, .94), 
                  y = c(1, 1, 1, .84, .84, .66, .66, .41))

setwd("~/Desktop/MSE/Model/H2/Figures/figure")
jpeg("Data_visulization_log.jpeg", height = 24, width = 18, units='in', res=400)
p
dev.off()
