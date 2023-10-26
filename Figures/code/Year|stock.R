library(ggplot2)
library(ggforce)
library(dplyr)
cbPalette2 <- c("grey", "black")

res = inla.collect.results("inla.model")

res_year <- res$summary.random$year
colnames(res_year)[4:6] <- c("p025", "p50", "p975")
res_year$stock_code <- rep(1:420, each=59)
res_year$year <- rep(1964:2022, times=420)

key <- unique(paste(dat$stock, dat$year, sep="_"))

stockid <- unique(dat[,c(12,3,1,10)])
stockid$stock_old <- stockid$stock
cc <- data.frame(table(stockid$ts_stock))
id <- cc[cc$Freq>1,]$Var1
stockid[stockid$ts_stock %in% id,]$stock <- c("Atlantic cod IIIa (west) and IV-VIId_IV-VIId-IIIaW_full", "Atlantic cod IIIa (west) and IV-VIId_IIIa_sub1",           
                                              "Atlantic cod IIIa (west) and IV-VIId_IV_sub2", "Atlantic cod IIIa (west) and IV-VIId_VIId_sub3",           
                                              "European Plaice North Sea_IV_full", "European Plaice North Sea_IV+III_full",                
                                              "Pacific hake Pacific Coast_full", "Pacific hake Pacific Coast_US_sub",                       
                                              "Pacific hake Pacific Coast_Canada_sub2", "Scup Atlantic Coast_USA-full1",                  
                                              "Scup Atlantic Coast_full2", "Sea scallop Georges Bank_a_full",                          
                                              "Sea scallop Georges Bank_a,b_pooled", "Snowy grouper Southern Atlantic coast_sub1",
                                              "Snowy grouper Southern Atlantic coast_sub2")

stockid[!(stockid$ts_stock %in% id),]$stock <- stockid[!(stockid$ts_stock %in% id),]$ts_stock

pdat <- merge(res_year, stockid, by="stock_code", all.x=TRUE)
pdat$key <- paste(pdat$stock_old, pdat$year, sep="_")
pdat$group <- 1
pdat[pdat$key %in% key,]$group <- 2
pdat$group <- as.factor(pdat$group)
pdat <- pdat[pdat$group==2,]

p <- ggplot(pdat, aes(x=year)) +
  geom_errorbar(aes(ymin=p025, ymax=p975), width=0, size=1, position=position_dodge(1), col="grey") +
  geom_point(aes(y=p50), size=3, position=position_dodge(1), col="black") +
  facet_wrap_paginate(~stock, scales="free_y", ncol = 5, nrow=4, labeller = label_wrap_gen(width=25), strip.position = "top", page=21) + 
  labs(x="Year", y="f(Year)") +
  scale_colour_manual(values=cbPalette2) +
  geom_hline(data = data.frame(yint=0), aes(yintercept = yint), linetype = "dotted", col="blue", size=1) +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_line(colour = "black"),
        axis.text.y   = element_text(size=20),
        axis.text.x   = element_text(size=20),
        axis.title.y  = element_text(size=22),
        axis.title.x  = element_blank(),
        strip.text.x = element_text(size=16, face ="bold"),
        strip.text.y = element_text(size=16, face ="bold"),
        strip.background = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
        panel.spacing = unit(1, "lines"),
        axis.ticks.length.x = unit(7, "pt"),
        axis.ticks.length.y = unit(7, "pt")) 

for(i in 1:n_pages(p)){
  p_save <-  p + 
    facet_wrap_paginate(~ stock, scales="free_y", ncol = 5, nrow = 4, labeller = label_wrap_gen(width=25), page = i)
  ggsave(plot = p_save, filename = paste0('YearEffects1/page_', i, '.jpg'), 
         height = 12, width = 20, units='in', dpi=400)
}

###########################################################################################################################

## Increase or decrease trends of year effects for each stock
pdat$year_rank <- NA
for (i in 1:420){
  pdat[pdat$stock_code==i,]$year_rank <- as.numeric(as.factor(pdat[pdat$stock_code==i,]$year))
}

pdat$year_effs_change <-NA
for (i in 1:420){
  for (j in 2:max(pdat[pdat$stock_code==i,]$year_rank)){
    pdat[pdat$stock_code==i & pdat$year_rank==j,]$year_effs_change <- pdat[pdat$stock_code==i & pdat$year_rank==j,]$p50 - pdat[pdat$stock_code==i & pdat$year_rank==j-1,]$p50
  }
}

bb <- pdat[!is.na(pdat$year_effs_change),]

bb_stock <- bb %>%
  group_by(stock) %>%
  summarize(bb_median=median(year_effs_change),
            bb_p025=quantile(year_effs_change, 0.025),
            bb_p975=quantile(year_effs_change, 0.975))

bb_stock$class <- 1
bb_stock[bb_stock$bb_p975 < 0,]$class <- 2  # significant decrease trend
bb_stock[bb_stock$bb_p025 > 0,]$class <- 3  # significant increase trend
bb_stock$class <- as.factor(bb_stock$class)

bb_stock <- bb_stock[order(bb_stock$bb_median),]
bb_stock$stock <- factor(bb_stock$stock, levels=unique(bb_stock$stock))
bb_stock$stock_code <- as.numeric(bb_stock$stock)

cbPalette2 <- c("dark grey", "blue", "red")

table(bb_stock$class)
#   1   2   3 
# 394  13  13

# significant decrease trend
bb_stock[bb_stock$class==2,]$stock
# [1] Gray triggerfish Gulf of Mexico                           
# [2] Argentine chub mackerel Southern Argentina                
# [3] Black Grouper Gulf of Mexico and South Atlantic           
# [4] Longnose skate Pacific Coast                              
# [5] Red grouper South Atlantic                                
# [6] Atlantic wolffish Gulf of Maine / Georges Bank            
# [7] Greater amberjack Gulf of Mexico                          
# [8] Spiny dogfish Scotian Shelf, Bay of Fundy and Georges Bank
# [9] Dusky rockfish Gulf of Alaska                             
# [10] Blueline tilefish Mid-Atlantic Bight                      
# [11] Monkfish Gulf of Maine / Northern Georges Bank            
# [12] Ocean quahog Mid-Atlantic Bight                           
# [13] Lingcod Northern Pacific Coast 

# significant increase trend
bb_stock[bb_stock$class==3,]$stock
# [1] Chilean jack mackerel Chilean EEZ and offshore               
# [2] Haddock Faroe Plateau                                        
# [3] Sand eel SA 1                                                
# [4] Yellowtail Snapper Southern Atlantic Coast and Gulf of Mexico
# [5] Mutton snapper Southern Atlantic coast and Gulf of Mexico    
# [6] Pacific herring West Coast of Vancouver Island               
# [7] Monkfish Southern Georges Bank / Mid-Atlantic                
# [8] Blacktip shark Gulf of Mexico                                
# [9] Atlantic cod Faroe Plateau                                   
# [10] small pelagics Zone C Morocco [pooled]                       
# [11] Lingcod Southern Pacific Coast                               
# [12] Red hake Southern Georges Bank / Mid-Atlantic                
# [13] Red snapper Southern Atlantic coast   

p <- ggplot(data=bb_stock, aes(x=stock_code, col=class)) +
  geom_errorbar(aes(ymin=bb_p025, ymax=bb_p975), width=0, size=1) +
  geom_point(aes(y=bb_median), size=3, shape=16) +
  scale_colour_manual(values=cbPalette2) +
  ylab("Year effect annual change") +
  geom_segment(x=-20, xend=-20, y=-4, yend=4, lwd=1.2, colour="grey30", lineend="square") +
  geom_segment(aes(x=1,xend=420, y=0, yend=0), linetype = "dashed", col="black", linewidth=1.2) +
  scale_y_continuous(breaks=seq(-4, 4, 2), limits=c(-4,4)) +
  scale_x_continuous(limits=c(1,420)) +
  geom_text(data = data.frame(x=35, y=-3, label="13 stocks with \n decreased trends"), aes(x=x, y=y, label=label), size=6, col="blue") +
  geom_text(data = data.frame(x=392, y=3, label="13 stocks with \n increase trends"), aes(x=x, y=y, label=label), size=6, col="red") +
  coord_flip() +
  theme_bw() +  
  theme_classic(base_size=12) +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y   = element_blank(),
    axis.text.x   = element_text(size=24),
    axis.title.y  = element_blank(),
    axis.title.x  = element_text(size=26),
    axis.ticks.y = element_blank(),
    legend.position="none",
    #plot.margin = unit(c(1,1,1,1), "lines"),
    #panel.spacing = unit(1, "lines"),
    axis.ticks.length.x = unit(7, "pt"),
    axis.ticks=element_line(size=0.65))

jpeg("Year_effs_change.jpeg", height = 10, width = 8, units='in', res=400)
p
dev.off()

###########################################################################################################################

## Positive stocks

ppdat <- pdat[pdat$stock %in% bb_stock[bb_stock$class==3,]$stock,]

p1 <- ggplot(ppdat, aes(x=year)) +
  geom_errorbar(aes(ymin=p025, ymax=p975), width=0, size=1, position=position_dodge(1), col="grey") +
  geom_point(aes(y=p50), size=3, position=position_dodge(1), col="black") +
  facet_wrap(~stock, scales="free_y", ncol = 5, nrow=3, labeller = label_wrap_gen(width=25), strip.position = "top") + 
  labs(x="Year", y="f(Year)") +
  scale_colour_manual(values=cbPalette2) +
  #geom_hline(data = data.frame(yint=0), aes(yintercept = yint), linetype = "dotted", col="blue", size=1) +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_line(colour = "black"),
        axis.text.y   = element_text(size=20),
        axis.text.x   = element_text(size=20),
        axis.title.y  = element_text(size=22),
        axis.title.x  = element_blank(),
        strip.text.x = element_text(size=16, face ="bold"),
        strip.text.y = element_text(size=16, face ="bold"),
        strip.background = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
        panel.spacing = unit(1, "lines"),
        axis.ticks.length.x = unit(7, "pt"),
        axis.ticks.length.y = unit(7, "pt")) 


jpeg("Year_effs_positive.jpeg", height = 12, width = 20, units='in', res=400)
p1
dev.off()


###########################################################################################################################

## Positive stocks

npdat <- pdat[pdat$stock %in% bb_stock[bb_stock$class==2,]$stock,]

p2 <- ggplot(npdat, aes(x=year)) +
  geom_errorbar(aes(ymin=p025, ymax=p975), width=0, size=1, position=position_dodge(1), col="grey") +
  geom_point(aes(y=p50), size=3, position=position_dodge(1), col="black") +
  facet_wrap(~stock, scales="free_y", ncol = 5, nrow=3, labeller = label_wrap_gen(width=25), strip.position = "top") + 
  labs(x="Year", y="f(Year)") +
  scale_colour_manual(values=cbPalette2) +
  #geom_hline(data = data.frame(yint=0), aes(yintercept = yint), linetype = "dotted", col="blue", size=1) +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_line(colour = "black"),
        axis.text.y   = element_text(size=20),
        axis.text.x   = element_text(size=20),
        axis.title.y  = element_text(size=22),
        axis.title.x  = element_blank(),
        strip.text.x = element_text(size=16, face ="bold"),
        strip.text.y = element_text(size=16, face ="bold"),
        strip.background = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
        panel.spacing = unit(1, "lines"),
        axis.ticks.length.x = unit(7, "pt"),
        axis.ticks.length.y = unit(7, "pt")) 


jpeg("Year_effs_negative.jpeg", height = 12, width = 20, units='in', res=400)
p2
dev.off()
