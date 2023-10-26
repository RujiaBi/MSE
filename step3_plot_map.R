# Clear workspace
rm(list = ls())

# Packages
library(maps)
library(mapdata)
library(rgdal)
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(freeR)
library(marineregions)  # data for lme, fao et al.
library(fields) 
library(sf)
library(abind)

# GIS packages
library(raster)
library(maptools)

# NetCDF packages
# ncdf no longer used; RNetCDF also an option
library(ncdf4)

# Define directories
datadir <- "~/Desktop/MSE/data_preparation/Data_for_map"
keydir <- "~/Desktop/Climate_Stock/Bi_Code/SST/stock_boundaries"
shpdir <- "~/Desktop/Climate_Stock/Bi_Code/SST/stock_boundaries/ramldb_boundaries"
lmedir <- "~/Desktop/MSE/data_preparation/Data_for_map/LME"
faodir <-  "~/Desktop/MSE/data_preparation/Data_for_map/FAO"
ramdir <-  "~/Desktop/MSE/data_preparation/Data_for_map/StockBoundaries"
outputdir <- "~/Desktop/MSE/data_preparation/Output"


# Read FAO data
fao_areas <- readOGR(dsn=faodir, laye="World_Fao_Zones", verbose=F)
#plot(fao_areas)

# Read LME data
lmes <- readOGR(dsn=lmedir, layer="LME66", verbose=F)

# Read stock boundaries data
ram <- readOGR(dsn=ramdir, layer="StockBoundaries", verbose=F)

# Read RAMLDB data
load("/Users/rujiabi/Desktop/Climate_Stock/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
stock_key <- stock  # 1374 stocks
area_key <- area 
all_stocks <- merge(stock_key, area_key, by="areaid", all.x=TRUE)
allstocks <- all_stocks[,c(2,6,4,5,7:9,13:17)]
colnames(allstocks)[5] <- "region_allstocks"
write.csv(allstocks, paste(datadir, "allstocks.csv", sep="/"), row.names=F)

# Read Mike-RAM data
mikram_all <- read.csv(paste(datadir, "Mike_RAM.csv", sep="/"), as.is=T)
mikram <- mikram_all[c(1:443), c(1:4)]
colnames(mikram)[3] <- "stocklong"
length(unique(mikram$stocklong))  # 388 stocks in RAM
length(unique(mikram$ts_stock))  # 443 ts_stocks
mikram_stockid <- merge(mikram, allstocks, by="stocklong", all.x=TRUE)
dim(mikram_stockid[!is.na(mikram_stockid$stockid),])  # 209 stocks with stockid

# Add shapefile for stocks with stockid
key <- read.csv(paste(keydir, "ramldb_stock_boundary_formatted.csv", sep="/"), as.is=T)
length(unique(key$stockid))  # 685 stocks
key <- subset(key, shp_source!="")

dd <- mikram_stockid %>% 
  # Add shapefile
  left_join(dplyr::select(key, assessid, stockid, shp_source, shp_path, zone_col, zones), by=c("stockid")) %>%  
  rename(assessid_shapefile=assessid)

stocks <- unique(dd[!is.na(dd$assessid_shapefile),]$assessid_shapefile)
#boundaries <- lapply(stocks, function(x)
#  readOGR(dsn=shpdir, p4s="+proj=longlat +datum=WGS84", layer=x, verbose=F))
# Replace attribute tables (because not uniform across shapefiles)
#boundaries_noatt <- boundaries
#for(i in 1:length(boundaries_noatt)){
#  print(paste(i, stocks[i]))
#  boundary <- boundaries_noatt[[i]]
#  boundary@data <- data.frame(assessid=stocks[i])
#  boundaries_noatt[[i]] <- boundary
#}
#boundaries_merged <- do.call(rbind, c(makeUniqueIDs = TRUE, boundaries_noatt))

# Export data
#save(boundaries_merged, file=paste(datadir, "198_stocks_boundaries_spdf.Rdata", sep="/"))
#writeOGR(obj=boundaries_merged, dsn=lmedir, layer="StockBoundaries", driver="ESRI Shapefile")

#################################################################################################

# Convert data to simple features format
lmes1 <- st_as_sf(lmes[,c(3)])
ram1 <- st_as_sf(ram)

# Check if the two objects have the same coordinate reference system
crs_match <- st_crs(lmes1) == st_crs(ram1)
print(crs_match)

# Check if each geometries is valid
st_is_valid(lmes1)
lmes1_1 <- lmes1[which(st_is_valid(lmes1)),]
lmes1_2 <- lmes1[-which(st_is_valid(lmes1)),]

st_is_valid(ram1)
ram1_1 <- ram1[which(st_is_valid(ram1)),]  # 164
ram1_2 <- ram1[-which(st_is_valid(ram1)),]  # 34

#plot(st_geometry(lmes1))
#plot(st_geometry(st_centroid(ram5)), pch=3, col="red", add=TRUE)

# Find centroids
#lmes_point <- st_centroid(lmes1)
ram_point_1 <- st_centroid(ram1_1)
ram_point_2 <- st_point_on_surface(ram1_2)

# See what LME polygon the RAM centroid is inside 
aa_1 <- st_join(lmes1_1, ram_point_1, join=st_covers)  
aa_df_1 <- aa_1 %>% st_set_geometry(NULL)
class(aa_df_1)
ram_with_lme_1 <- aa_df_1[!is.na(aa_df_1$assessid),]  # 119 stocks

aa_2 <- st_join(lmes1_1, ram_point_2, join=st_covers)  
aa_df_2 <- aa_2 %>% st_set_geometry(NULL)
class(aa_df_2)
ram_with_lme_2 <- aa_df_2[!is.na(aa_df_2$assessid),]  # 28 stocks

ram_with_lme <- rbind(ram_with_lme_1, ram_with_lme_2)


# Match the LME to mikram_stockid_assessid (dd)
dd$assessid <- dd$assessid_shapefile
mik_with_lme <- merge(dd, ram_with_lme, by="assessid", all.x=TRUE)
write.csv(mik_with_lme, paste(datadir, "2_mik_with_lme.csv", sep="/"), row.names=F)

# Add LME to more stocks
unique(mik_with_lme[is.na(mik_with_lme$LME_NAME),]$areaname)  # 46 areas and 1 NA
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Bering Sea and Aleutian Islands",]$LME_NAME <- "East Bering Sea"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Saint Matthews Island",]$LME_NAME <- "East Bering Sea"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Eastern Bering Sea and Aleutian Islands",]$LME_NAME <- "East Bering Sea"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Bristol Bay",]$LME_NAME <- "East Bering Sea"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Bering Sea",]$LME_NAME <- "East Bering Sea"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Aleutian Islands",]$LME_NAME <- "Aleutian Islands"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Eastern Bering Sea",]$LME_NAME <-  "East Bering Sea"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Gulf of Alaska",]$LME_NAME <- "Gulf of Alaska"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Southern Oceans",]$LME_NAME <- "Southern Oceans"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Macquarie Island",]$LME_NAME <-  "Southeast Australian Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Southeast Australia",]$LME_NAME <-  "Southeast Australian Shelf"

mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Shrimp Fishing Areas 2-3",]$LME_NAME <- "Labrador - Newfoundland"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Scotian Shelf and Southern Grand Banks",]$LME_NAME <- "Labrador - Newfoundland"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Scotian Shelf and Bay of Fundy",]$LME_NAME <- "Labrador - Newfoundland" 
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Scotian Shelf, Bay of Fundy and Georges Bank",]$LME_NAME <- "Labrador - Newfoundland" 
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Eastern Scotian Shelf (SFA 13-15)",]$LME_NAME <- "Labrador - Newfoundland" 
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Pacific Coast of Japan",]$LME_NAME <- "Kuroshio Current"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Tsushima Strait",]$LME_NAME <- "Kuroshio Current"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="South Atlantic",]$LME_NAME <- "AOHS"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="North Pacific",]$LME_NAME <- "POHS"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Pacific Ocean",]$LME_NAME <- "POHS"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="South Africa",]$LME_NAME <- "Agulhas Current"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="South Africa Subantarctic Prince Edward Islands",]$LME_NAME <- "Agulhas Current"

mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Atlantic Coast",]$LME_NAME <- "Northeast U.S. Continental Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Mid-Atlantic Coast",]$LME_NAME <- "Northeast U.S. Continental Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="New Zealand",]$LME_NAME <- "New Zealand Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="East and South Chatham Rise",]$LME_NAME <- "New Zealand Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Northwest Chatham Rise",]$LME_NAME <- "New Zealand Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="New Zealand Mid East Coast",]$LME_NAME <- "New Zealand Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Bay of Plenty",]$LME_NAME <- "New Zealand Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Chatham Rise",]$LME_NAME <- "New Zealand Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Sub-Antarctic",]$LME_NAME <- "New Zealand Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Atlantic",]$LME_NAME <- "Northeast U.S. Continental Shelf"

mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Iceland (Summer spawners)",]$LME_NAME <- "Iceland Shelf and Sea"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Iceland Grounds",]$LME_NAME <- "Iceland Shelf and Sea"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Campbell Island Rise",]$LME_NAME <- "New Zealand Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Southern Atlantic coast and Gulf of Mexico",]$LME_NAME <- "Southeast U.S. Continental Shelf"

mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Rockall Bank",]$LME_NAME <- "Celtic-Biscay Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Southern Argentina",]$LME_NAME <- "Patagonian Shelf"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Washington",]$LME_NAME <- "California Current"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Northern Pacific Coast",]$LME_NAME <- "California Current"

mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Southern Pacific Coast",]$LME_NAME <- "California Current"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Northern Chile",]$LME_NAME <- "Humboldt Current"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Central-Southern Chile",]$LME_NAME <- "Humboldt Current"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Southern Chile",]$LME_NAME <- "Humboldt Current"
mik_with_lme[!is.na(mik_with_lme$areaname) & mik_with_lme$areaname=="Sea of Japan North",]$LME_NAME <- "Sea of Japan"

#####################################################################################################################

unique(mik_with_lme[is.na(mik_with_lme$LME_NAME),]$region) 
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="Canada west",]$LME_NAME <- "Gulf of Alaska"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="Canada east",]$LME_NAME <- "Labrador - Newfoundland"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="US west",]$LME_NAME <- "California Current"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="US Alaska",]$LME_NAME <- "Gulf of Alaska"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="Europe-ICES-EU",]$LME_NAME <- "Celtic-Biscay Shelf"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="Australia",]$LME_NAME <- "Southeast Australian Shelf"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="South Africa",]$LME_NAME <- "Agulhas Current"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="New Zealand",]$LME_NAME <- "New Zealand Shelf"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="Japan",]$LME_NAME <- "Kuroshio Current"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="West Africa",]$LME_NAME <- "Canary Current"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="US northeast",]$LME_NAME <- "Northeast U.S. Continental Shelf"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="high seas",]$LME_NAME <- "AOHS"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="Europe-ICES-nonEU",]$LME_NAME <- "Iceland Shelf and Sea" 
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="US southeast",]$LME_NAME <- "Southeast U.S. Continental Shelf"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="South America",]$LME_NAME <- "Humboldt Current"
mik_with_lme[is.na(mik_with_lme$LME_NAME) & mik_with_lme$region=="Russia east",]$LME_NAME <- "West Bering Sea"

  
## Match LME to 1_dat_for_model.csv
mik_with_lme <- mik_with_lme[,c(4,22)]
dat <- read.csv(paste(outputdir, "1_dat_for_model.csv", sep="/"), as.is=T)
dat1 <- merge(dat, mik_with_lme, by="ts_stock", all.x=TRUE)
dat1[is.na(dat1$LME_NAME),]$LME_NAME <- "Northeast U.S. Continental Shelf"
length(unique(dat1$stock))  # 420 stocks
write.csv(dat1, paste(datadir, "1_dat_for_model_with_LME.csv", sep="/"), row.names=F)
dat1 <- read.csv(paste(datadir, "1_dat_for_model_with_LME.csv", sep="/"), as.is=T)
dat1$log_tac_catch <- log(dat1$tac_change+1)

# Summarize data
dat3 <- dat1 %>% 
  group_by(LME_NAME) %>% 
  summarize(nstocks=n_distinct(stock),
            average_catch_by_TAC=mean(catch_by_TAC),
            average_tac_change=mean(tac_change),
            average_log_tac_change=mean(log_tac_catch))

dim(dat3)  # 29 LMEs


# Format data for plotting
################################################################################

# Add colors
summary(dat3$average_log_tac_change)
hist(dat3$average_log_tac_change, n=15)
dat3$perc_bin <- cut(dat3$average_log_tac_change, breaks=seq(-0.3,0.3,0.01))
colors_tac_change <- colorRampPalette(brewer.pal(11,"RdBu"))(nlevels(dat3$perc_bin))
colors_tac_change_tr <- rgb(t(col2rgb(colors_tac_change))/255, alpha=0.7)
dat3$perc_bin_color <- colors_tac_change_tr[dat3$perc_bin]

# read position for LME 
lme_key_66 <- read.csv(paste(datadir, "lmes_point.csv", sep="/"), as.is=T)
lme_key_66[lme_key_66$LME_NAME=="East Bering Sea",]$long_dd <- -175
lme_key_hs <- data.frame(matrix(NA, nrow=3, ncol=3))
colnames(lme_key_hs) <- colnames(lme_key_66)
lme_key_hs$LME_NAME <- c("POHS", "AOHS", "Southern Oceans")
lme_key_hs$long_dd <- c(-130, -14, -5)
lme_key_hs$lat_dd <- c(-10, -8, -68)
lme_key <- rbind(lme_key_66, lme_key_hs)

# Add plotting position (pos) and plotting label to data
dat3 <- dat3 %>% 
  left_join(dplyr::select(lme_key, LME_NAME, lat_dd, long_dd), by=c("LME_NAME")) %>% 
  mutate(pos=revalue(LME_NAME, c("AOHS"=1,
                                 "Agulhas Current"=4,
                                 "Aleutian Islands"=1,
                                 "Benguela Current"=1,   
                                 "Black Sea"=4,
                                 "California Current"=2,
                                 "Canary Current"=1,
                                 "Celtic-Biscay Shelf"=4,
                                 "East Bering Sea"=1,
                                 "Faroe Plateau"=2,
                                 "Gulf of Alaska"=1,
                                 "Gulf of Mexico"=1,
                                 "Humboldt Current"=2,
                                 "Iceland Shelf and Sea"=2,
                                 "Kuroshio Current"=4,
                                 "Labrador - Newfoundland"=2,
                                 "New Zealand Shelf"=3,
                                 "North Sea"=4,
                                 "Northeast U.S. Continental Shelf"=2,
                                 "Norwegian Sea"=4,
                                 "POHS"=1,
                                 "Patagonian Shelf"=4,
                                 "Scotian Shelf"=2,
                                 "Sea of Japan"=2,
                                 "Sea of Okhotsk"=2,
                                 "Southeast Australian Shelf"=2,
                                 "Southeast U.S. Continental Shelf"=4,
                                 "Southern Oceans"=1,
                                 "West Bering Sea"=3)),
         label=revalue(LME_NAME, c("AOHS"="Atlantic Ocean",
                                   "Agulhas Current"="Agulhas Current",
                                   "Aleutian Islands"="Aleutian Islands",
                                   "Benguela Current"="Benguela Current",   
                                   "Black Sea"="Black Sea",
                                   "California Current"="CA Current",
                                   "Canary Current"="Canary Current",
                                   "Celtic-Biscay Shelf"="Celtic-Biscay Shelf",
                                   "East Bering Sea"="E Bering Sea",
                                   "Faroe Plateau"="Faroe Plateau",
                                   "Gulf of Alaska"="Gulf of Alaska",
                                   "Gulf of Mexico"="Gulf of Mexico",
                                   "Humboldt Current"="Humboldt Current",
                                   "Iceland Shelf and Sea"="Iceland Shelf & Sea",
                                   "Kuroshio Current"="Kuroshio Current",
                                   "Labrador - Newfoundland"="Labrador-Newfoundland",
                                   "New Zealand Shelf"="NZ Shelf",
                                   "North Sea"="North Sea",
                                   "Northeast U.S. Continental Shelf"="NE US Shelf",
                                   "Norwegian Sea"="Norwegian Sea",
                                   "POHS"="Pacific Ocean",
                                   "Patagonian Shelf"="Patagonian Shelf",
                                   "Scotian Shelf"="Scotian Shelf",
                                   "Sea of Japan"="Sea of Japan",
                                   "Sea of Okhotsk"="Sea of Okhotsk",
                                   "Southeast Australian Shelf"="SE Austr. Shelf",
                                   "Southeast U.S. Continental Shelf"="SE US Shelf",
                                   "Southern Oceans"="Southern Ocean",
                                   "West Bering Sea"="W Bering Sea")))


# Plot data
################################################################################

# Setup figure
figname <- "Map.png"
png(paste(outputdir, figname, sep="/"), width=6.5, height=3, units="in", res=600)
layout(matrix(c(1,2), ncol=2, byrow=T), widths=c(0.88,0.12))
par(mar=c(0.1,0.1,0.1,0.1), xpd=NA)

# Parameters
xlim <- c(-180, 180)
ylim <- c(-90, 90)

# Plot FAO areas
plot(fao_areas, border="grey70", lty=3, xlim=xlim, ylim=ylim)

# Plot world countries
map("world", col="grey85", fill=T, border="white", lwd=0.3,
    xlim=xlim, ylim=ylim, add=T)

# Add Catch/TAC change
# barplot(hist(data$msy_tmt, breaks=c(0, 100, 500, 1000, 2000, 10000, 20000), plot=F)$counts)
catch_by_TAC_breaks <-  c(0.5, 1, 1.5)
catch_by_TAC <- cut(dat3$average_catch_by_TAC, breaks=c(0, catch_by_TAC_breaks))
wts <- seq(1.8,4,length.out=nlevels(catch_by_TAC))
ct_wts <- wts[catch_by_TAC]
points(x=dat3$long_dd, y=dat3$lat_dd, 
       pch=21, bg=dat3$perc_bin_color, col="grey40", cex=ct_wts)

# LME names
text(x=dat3$long_dd, y=dat3$lat_dd, pos=dat3$pos, offset=0.6,
     labels=dat3$label, cex=0.52, font=3)

# Add number of stocks
text(x=dat3$long_dd, y=dat3$lat_dd, labels=dat3$nstocks, cex=0.5, font=2, col="black")

# Catch/TAC size legend
points(x=rep(205, length(catch_by_TAC_breaks)), 
       y=56+seq(0,5.5, length.out=length(catch_by_TAC_breaks)), cex=wts, lwd=0.6)
text(x=190, y=84, pos=4, labels="Catch/ACL", font=2, xpd=NA, cex=0.6)
text(x=210, y=46+seq(0,27, length.out=length(catch_by_TAC_breaks)),
     pos=4, labels=catch_by_TAC_breaks, xpd=NA, cex=0.6)

# Legend
plot(1:10, 1:10, bty="n", type="n", xaxt="n", yaxt="n", xlab="", ylab="")
#colorbar.plot(x=0, y=1.25, adj.x=0, adj.y=0, horiz=F, col=colors_tac_change,
#              strip=seq(-1.5, 1.5, length.out=nlevels(dat3$perc_bin)), 
#              strip.width=0.3, strip.length=2, xpd=NA)
colorbar.plot(x=0, y=1.4, adj.x=0, adj.y=0, horiz=F, col=colors_tac_change,
              strip=seq(-1.5,1.5, length.out=nlevels(dat3$perc_bin)), 
              strip.width=0.3, strip.length=2, xpd=NA)
text(x=2, y=c(1.4, 4.225, 6.2), pos=4, labels=c("-0.3", "0", "0.3"), cex=0.6)
text(x=-3, y=6.6, pos=4, labels="log(ACL change+1)", font=2, cex=0.6)

# Off
dev.off()
graphics.off()

