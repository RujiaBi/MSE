# Define directories
datadir <- "~/Desktop/MSE/data_preparation/Data"
outputdir <- "~/Desktop/MSE/data_preparation/Output"

# region key
region_key <- data.frame(matrix(NA, nrow=24, ncol=2))
colnames(region_key) <- c("council_code", "Region")
region_key$council_code <- c(1:24)
region_key$Region <- c("AOHSs", "ASMFC", "Australia", "Canada East", "Canada West", "Europe-ICES-EU", "Europe-ICES-nonEU", 
                       "Europe-MBS", "GMFMC", "IPHC", "Japan", "MAFMC", "NEFMC", "New Zealand", "NPFMC", "PFMC", "POHSs", 
                       "Russia East", "SAFMC", "SOHSs", "South Africa",  "South America", "US Non-Federal", "West Africa")


# Read data
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



# Plot
library(ggplot2)
library(ggsci)  # color
library(ggpubr)
library(cowplot)
cbPalette2 <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
                "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
                "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D")


p1 <- ggplot(dd) +
 # geom_segment(aes(x=tmin, y=stock_code, xend=tmax, yend=stock_code, colour=Region), linewidth=1) + 
  geom_point(aes(year, stock_code_new, colour=Region), size=1.5, shape = 19) +
  scale_colour_manual(values=cbPalette2) +
  ylab("Stock count") +
  theme_bw() + 
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.line.x = element_line(colour = "black"),
    axis.text.y   = element_text(size=26),
    axis.text.x   = element_text(size=26),
    axis.title.y  = element_text(size=26),
    axis.title.x  = element_blank(),
    legend.title=element_text(size=23),
    legend.text=element_text(size=21),
    legend.box="vertical", 
    legend.margin=margin(20,0,0,0),
    legend.spacing.y = unit(0.8, "lines"),
    strip.text.x = element_text(size=36),
    strip.text.y = element_text(size=36),
    strip.background = element_blank(),
    plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
    panel.spacing = unit(1, "lines"),
    axis.ticks.length.x = unit(15, "pt"),
    axis.ticks.length.y = unit(15, "pt")) +
  guides(colour = guide_legend(ncol = 1, bycol = T, override.aes = list(size=3)))


p2 <- ggplot(cc) +
  #geom_errorbarh(aes(xmax=tmax, xmin=tmin, y=region_code, colour=Region, height=0), size=1.5) +
  geom_point(aes(year, region_code, colour=Region), size=2.5, shape = 19) +
  scale_colour_manual(values=cbPalette2) +
  scale_y_reverse() + 
  ylab("Region") +
  theme_bw() + 
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.line.x = element_line(colour = "black"),
    axis.text.y   = element_text(size=26),
    axis.text.x   = element_text(size=26),
    axis.title.y  = element_text(size=26),
    axis.title.x  = element_blank(),
    legend.title=element_text(size=23),
    legend.text=element_text(size=21),
    legend.box="vertical", 
    legend.margin=margin(20,0,0,0),
    legend.spacing.y = unit(0.8, "lines"),
    strip.text.x = element_text(size=36),
    strip.text.y = element_text(size=36),
    strip.background = element_blank(),
    plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
    panel.spacing = unit(1, "lines"),
    axis.ticks.length.x = unit(15, "pt"),
    axis.ticks.length.y = unit(15, "pt")) +
  guides(colour = guide_legend(ncol = 1, bycol = T, override.aes = list(size=3)))

legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.box.margin = margin(20,0,0,0))
)

p <- ggdraw() +
  draw_plot(p1 + theme(legend.position="none"), x = 0, y = 0, width = .5, height = 1) +
  draw_plot(p2 + theme(legend.position="none"), x = .5, y = 0, width = .5, height = 1) +
  draw_plot_label(label = c("A", "B"), size = 30,
                  x = c(0, 0.5), y = c(1, 1))


setwd(outputdir)
jpeg("YearTrend.jpeg", height = 10, width = 16, units='in', res=400)
plot_grid(p, legend, rel_widths = c(0.8, 0.2))
dev.off()

