# Define directories
datadir <- "~/Desktop/MSE/data_preparation/Data"
outputdir <- "~/Desktop/MSE/data_preparation/Output"

#############################################################################

# Read Mike's data
mik <- read.csv(paste(datadir, "snapp_dat.csv", sep="/"), as.is=T)
colnames(mik)
# Cpair=Catch, TAC=ACL, Cadvised=ABC
mik$stock <- paste(mik$ts_stock, mik$subarea, mik$nesting_level, sep="_")
length(unique(mik$stock))  # 470 stocks
dim(mik)  # 11266 rows
mik$source <- "Mike's data"

#############################################################################

# Read NOAA-EDAB data
edab <- read.csv(paste(datadir, "abc.acl.csv", sep="/"), as.is=T)
dim(edab)  # 1930
summary(edab)
edab1 <- edab[edab$Stock3=="",]; edab1 <- edab1[,c(-4)]; colnames(edab1) <- c("Time", "Stock", "Type", "Value", "EPU", "Units" )
edab2 <- edab[edab$Stock3!="",]; edab2 <- edab2[,c(-2)]; colnames(edab2) <- c("Time", "Stock", "Type", "Value", "EPU", "Units" )
edab <- rbind(edab1, edab2)
edab$stock <- paste(edab$Stock, edab$EPU, sep="-")

length(unique(edab$stock))  # 52 stocks

# keep unique rows
edab <- unique(edab)
dim(edab)  # 1641 

# reformat
edab$key <- paste(edab$stock, edab$Time, sep="_")

catch <- edab[edab$Type=="Catch", c(8,7,1,4)]
catch$catch <- catch$Value
dim(catch)  # 480
length(unique(catch$key))  # 480

acl <- edab[edab$Type=="Quota" | edab$Type=="ACL", c(8,7,1,4)]
acl$acl <- acl$Value
dim(acl)  # 485
length(unique(acl$key))  # 485

olf <- merge(catch[c(1,2,3,5)], acl[,c(1,5)], by="key", all=TRUE)
dim(olf)  # 486, delete 33 rows without Catch and 28 rows without ACL
summary(olf)
olf <- olf[!is.na(olf$catch) & !is.na(olf$acl),]
olf$catch_by_TAC <- olf$catch/olf$acl
length(unique(olf$stock))  # 48 stocks

unique(olf$stock)

# [1] "Acadian redfish - Gulf of Maine / Georges Bank-NE"
dd_s1 <- olf[olf$stock=="Acadian redfish - Gulf of Maine / Georges Bank-NE",]
dd_s1$ts_stock <- "Acadian redfish Gulf of Maine / Georges Bank"
dd_s1$stock <- "Acadian redfish Gulf of Maine / Georges Bank_USA-NMFS-5YZ_full"
dd_s1 <- dd_s1[dd_s1$Time > 2014,]
dd_s1$DemersPelag <- "demersal"

# [2] "American plaice - Gulf of Maine / Georges Bank-NE" 
dd_s2 <- olf[olf$stock=="American plaice - Gulf of Maine / Georges Bank-NE",]
dd_s2$ts_stock <- "American Plaice NAFO-5YZ"
dd_s2$stock <- "American Plaice NAFO-5YZ_USA-NMFS-5YZ_full"
dd_s2 <- dd_s2[dd_s2$Time > 2014,]
dd_s2$DemersPelag <- "demersal"

mik <- mik[mik$stock != "American Plaice NAFO-5YZ_USA-NMFS-5YZ_full" | (mik$stock=="American Plaice NAFO-5YZ_USA-NMFS-5YZ_full" & mik$year < 2015),]

# [3] "Atlantic cod - Georges Bank-NE" 
# Quite different values from Mike's olfa!

# [4] "Atlantic cod - Gulf of Maine-NE" 
dd_s4 <- olf[olf$stock=="Atlantic cod - Gulf of Maine-NE",]
dd_s4$ts_stock <- "Atlantic cod Gulf of Maine"
dd_s4$stock <- "Atlantic cod Gulf of Maine_USA-NMFS-5Y_full"
dd_s4 <- dd_s4[dd_s4$Time > 2014,]
dd_s4$DemersPelag <- "benthopelagic"

mik <- mik[mik$stock != "Atlantic cod Gulf of Maine_USA-NMFS-5Y_full" | (mik$stock=="Atlantic cod Gulf of Maine_USA-NMFS-5Y_full" & mik$year < 2015),]

# [5] "Atlantic halibut - Northwestern Atlantic Coast-NE" 
dd_s5 <- olf[olf$stock=="Atlantic halibut - Northwestern Atlantic Coast-NE",]
dd_s5$ts_stock <- "Atlantic Halibut NAFO-5YZ"
dd_s5$stock <- "Atlantic Halibut NAFO-5YZ_USA-NMFS-5YZ_full"
dd_s5 <- dd_s5[dd_s5$Time > 2014,]
dd_s5$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Atlantic Halibut NAFO-5YZ_USA-NMFS-5YZ_full" | (mik$stock=="Atlantic Halibut NAFO-5YZ_USA-NMFS-5YZ_full" & mik$year < 2015),]

# [6] "Atlantic Herring-NE" 
dd_s6 <- olf[olf$stock=="Atlantic Herring-NE",]
dd_s6$ts_stock <- "Atlantic herring Northwestern Atlantic Coast"
dd_s6$stock <- "Atlantic herring Northwestern Atlantic Coast_USA-NMFS-NWATLC_sub"
dd_s6 <- dd_s6[dd_s6$Time > 2014,]
dd_s6$DemersPelag <- "benthopelagic"

# [7] "Atlantic Mackerel-MAB" 
dd_s7 <- olf[olf$stock=="Atlantic Mackerel-MAB",]
dd_s7$ts_stock <- "Atlantic Mackerel Mid-Atlantic Bight"
dd_s7$stock <- "Atlantic Mackerel Mid-Atlantic Bight"
dd_s7$DemersPelag <- "pelagic-neritic"

# [8] "Atlantic Sea Scallop-NE"  
dd_s8 <- olf[olf$stock=="Atlantic Sea Scallop-NE",]
dd_s8$ts_stock <- "Sea scallop Georges Bank and Mid-Atlantic Bight"
dd_s8$stock <- "Sea scallop Georges Bank and Mid-Atlantic Bight__full"
dd_s8 <- dd_s8[dd_s8$Time > 2014,]
dd_s8$DemersPelag <- "benthic"

# [9] "Atlantic wolffish - Gulf of Maine / Georges Bank-NE" 
dd_s9 <- olf[olf$stock=="Atlantic wolffish - Gulf of Maine / Georges Bank-NE",]
dd_s9$ts_stock <- "Atlantic wolffish Gulf of Maine / Georges Bank"
dd_s9$stock <- "Atlantic wolffish Gulf of Maine / Georges Bank"
dd_s9$DemersPelag <- "demersal"

# [10] "Black Sea Bass Commercial-MAB"    
# [11] "Black Sea Bass Recreational-MAB"  
# Different from Mike's values.

# [12] "Bluefish-MAB"         
# Quite different from Mike's values.

# [13] "Blueline Tilefish Commercial-MAB"                            
# [14] "Blueline Tilefish Recreational-MAB"     
dd_s13_14 <- olf[olf$stock=="Blueline Tilefish Commercial-MAB",]
dd_s13_14$ts_stock <- "Blueline tilefish Mid-Atlantic Bight"
dd_s13_14$stock <- "Blueline tilefish Mid-Atlantic Bight"
dd_s13_14$catch <- olf[olf$stock=="Blueline Tilefish Commercial-MAB",]$catch + olf[olf$stock=="Blueline Tilefish Recreational-MAB",]$catch
dd_s13_14$acl <- olf[olf$stock=="Blueline Tilefish Commercial-MAB",]$acl + olf[olf$stock=="Blueline Tilefish Recreational-MAB",]$acl
dd_s13_14$catch_by_TAC <- dd_s13_14$catch/dd_s13_14$acl
dd_s13_14$DemersPelag <- "demersal"

# [15] "Butterfish-MAB"         
dd_s15 <- olf[olf$stock=="Butterfish-MAB",]
dd_s15$ts_stock <- "Atlantic butterfish Gulf of Maine / Cape Hatteras"
dd_s15$stock <- "Atlantic butterfish Gulf of Maine / Cape Hatteras_USA-NMFS-NWATLC_full1"
dd_s15 <- dd_s15[dd_s15$Time > 2015,]
dd_s15$DemersPelag <- "benthopelagic"

# [16] "Chub Mackerel-MAB"        
dd_s16 <- olf[olf$stock=="Chub Mackerel-MAB",]
dd_s16$ts_stock <- "Chub mackerel Mid-Atlantic Bight"
dd_s16$stock <- "Chub mackerel Mid-Atlantic Bight"
dd_s16$DemersPelag <- "pelagic-neritic"

# [17] "Golden Tilefish-MAB"   
dd_s17 <- olf[olf$stock=="Golden Tilefish-MAB",]
dd_s17$ts_stock <- "Tilefish Mid-Atlantic Coast"
dd_s17$stock <- "Tilefish Mid-Atlantic Coast_USA-NMFS-MATLC_full"
dd_s17 <- dd_s17[dd_s17$Time > 2015,]
dd_s17$DemersPelag <- "demersal"

# [18] "Goosefish - Gulf of Maine / Northern Georges Bank-NE"  
dd_s18 <- olf[olf$stock=="Goosefish - Gulf of Maine / Northern Georges Bank-NE",]
dd_s18$ts_stock <- dd_s18$stock <- "Goosefish Gulf of Maine / Northern Georges Bank"
dd_s18$DemersPelag <- "demersal"

# [19] "Goosefish - Southern Georges Bank / Mid-Atlantic-NE"       
dd_s19 <- olf[olf$stock=="Goosefish - Southern Georges Bank / Mid-Atlantic-NE",]
dd_s19$ts_stock <- dd_s19$stock <- "Goosefish Southern Georges Bank / Mid-Atlantic"
dd_s19$DemersPelag <- "demersal"

# [20] "Haddock - Georges Bank-NE"     
dd_s20 <- olf[olf$stock=="Haddock - Georges Bank-NE",]
dd_s20$ts_stock <- dd_s20$stock <- "Haddock Georges Bank"
dd_s20$DemersPelag <- "demersal"

# [21] "Haddock - Gulf of Maine-NE"     
dd_s21 <- olf[olf$stock=="Haddock - Gulf of Maine-NE",]
dd_s21$ts_stock <- "Haddock NAFO-5Y"
dd_s21$stock <- "Haddock NAFO-5Y_USA-NMFS-5Y_full"
dd_s21 <- dd_s21[dd_s21$Time > 2014,]
dd_s21$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Haddock NAFO-5Y_USA-NMFS-5Y_full" | (mik$stock=="Haddock NAFO-5Y_USA-NMFS-5Y_full" & mik$year < 2015),]

# [22] "Illex Squid-MAB" 
dd_s22 <- olf[olf$stock=="Illex Squid-MAB",]
dd_s22$ts_stock <- dd_s22$stock <- "Illex squid Mid-Atlantic Bight"
dd_s22$DemersPelag <- "pelagic"

# [23] "Longfin Squid-MAB"  
dd_s23 <- olf[olf$stock=="Longfin Squid-MAB",]
dd_s23$ts_stock <- "Longfin inshore squid Atlantic Coast"
dd_s23$stock <- "Longfin inshore squid Atlantic Coast_USA-NMFS-ATLC_full"
dd_s23 <- dd_s23[dd_s23$Time > 2014,]
dd_s23$DemersPelag <- "benthopelagic"

# [24] "Ocean pout - Northwestern Atlantic Coast-NE" 
dd_s24 <- olf[olf$stock=="Ocean pout - Northwestern Atlantic Coast-NE",]
dd_s24$ts_stock <- "Ocean pout Northwestern Atlantic Coast"
dd_s24$stock <- "Ocean pout Northwestern Atlantic Coast_USA-NMFS-NWATLC_full"
dd_s24 <- dd_s24[dd_s24$Time > 2014,]
dd_s24$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Ocean pout Northwestern Atlantic Coast_USA-NMFS-NWATLC_full" | (mik$stock=="Ocean pout Northwestern Atlantic Coast_USA-NMFS-NWATLC_full" & mik$year < 2015),]

# [25] "Ocean Quahog-MAB"    
dd_s25 <- olf[olf$stock=="Ocean Quahog-MAB",]
dd_s25$ts_stock <- dd_s25$stock <- "Ocean quahog Mid-Atlantic Bight"
dd_s25$DemersPelag <- "benthic"

# [26] "Pollock - Gulf of Maine / Georges Bank-NE"       
dd_s26 <- olf[olf$stock=="Pollock - Gulf of Maine / Georges Bank-NE",]
dd_s26$ts_stock <- "Pollock NAFO-5YZ"
dd_s26$stock <- "Pollock NAFO-5YZ_USA-NMFS-5YZ_full"
dd_s26 <- dd_s26[dd_s26$Time > 2014,]
dd_s26$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Pollock NAFO-5YZ_USA-NMFS-5YZ_full" | (mik$stock=="Pollock NAFO-5YZ_USA-NMFS-5YZ_full" & mik$year < 2015),]

# [27] "Red Crab-NE"   
dd_s27 <- olf[olf$stock=="Red Crab-NE",]
dd_s27$ts_stock <- dd_s27$stock <- "Red crab US northeast"
dd_s27$DemersPelag <- "benthic"

# [28] "Red hake - Gulf of Maine / Northern Georges Bank-NE"       
dd_s28 <- olf[olf$stock=="Red hake - Gulf of Maine / Northern Georges Bank-NE",]
dd_s28$ts_stock <- dd_s28$stock <- "Red hake Gulf of Maine / Northern Georges Bank"
dd_s28$DemersPelag <- "demersal"

# [29] "Red hake - Southern Georges Bank / Mid-Atlantic-NE" 
dd_s29 <- olf[olf$stock=="Red hake - Southern Georges Bank / Mid-Atlantic-NE",]
dd_s29$ts_stock <- dd_s29$stock <- "Red hake Southern Georges Bank / Mid-Atlantic"
dd_s29$DemersPelag <- "demersal"

# [30] "Scup Commercial-MAB"                                         
# [31] "Scup Recreational-MAB"    
# Different from Mike's values!

# [32] "Silver hake - Gulf of Maine / Northern Georges Bank-NE"    
dd_s32 <- olf[olf$stock=="Silver hake - Gulf of Maine / Northern Georges Bank-NE",]
dd_s32$ts_stock <- dd_s32$stock <- "Silver hake Gulf of Maine / Northern Georges Bank"
dd_s32$DemersPelag <- "demersal"

# [33] "Skates (Wings)*-NE"      
dd_s33 <- olf[olf$stock=="Skates (Wings)*-NE",]
dd_s33$ts_stock <- dd_s33$stock <- "Skates US Northeast"
dd_s33$DemersPelag <- "demersal"

# [34] "Spiny Dogfish-MAB" 
# [35] "Spiny Dogfish-NE"           
# Different from Mike's olfa.

# [36] "Summer Flounder Commercial-MAB"                              
# [37] "Summer Flounder Recreational-MAB"  
dd_s36_37 <- olf[olf$stock=="Summer Flounder Commercial-MAB",]
dd_s36_37$ts_stock <- "Summer flounder Mid-Atlantic Coast"
dd_s36_37$stock <- "Summer flounder Mid-Atlantic Coast_USA-NMFS-MATLC_full"
dd_s36_37$catch <- olf[olf$stock=="Summer Flounder Commercial-MAB",]$catch + olf[olf$stock=="Summer Flounder Recreational-MAB",]$catch
dd_s36_37$acl <- olf[olf$stock=="Summer Flounder Commercial-MAB",]$acl + olf[olf$stock=="Summer Flounder Recreational-MAB",]$acl
dd_s36_37$catch_by_TAC <- dd_s36_37$catch/dd_s36_37$acl
dd_s36_37 <- dd_s36_37[dd_s36_37$Time > 2014,]
dd_s36_37$DemersPelag <- "demersal"

# [38] "Surfclam-MAB" 
# Different from Mike's olfa.

# [39] "White hake - Gulf of Maine / Georges Bank-NE"       
dd_s39 <- olf[olf$stock=="White hake - Gulf of Maine / Georges Bank-NE",]
dd_s39$ts_stock <- "White hake Georges Bank / Gulf of Maine"
dd_s39$stock <- "White hake Georges Bank / Gulf of Maine_USA-NMFS-5YZ_full"
dd_s39 <- dd_s39[dd_s39$Time > 2015,]
dd_s39$DemersPelag <- "demersal"

# [40] "Windowpane - Gulf of Maine / Georges Bank-NE"         
dd_s40 <- olf[olf$stock=="Windowpane - Gulf of Maine / Georges Bank-NE",]
dd_s40$ts_stock <- "Windowpane flounder - Gulf of Maine / Georges Bank"
dd_s40$stock <- "Windowpane flounder - Gulf of Maine / Georges Bank_USA-NMFS-5YZ_full"
dd_s40 <- dd_s40[dd_s40$Time > 2014,]
dd_s40$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Windowpane flounder - Gulf of Maine / Georges Bank_USA-NMFS-5YZ_full" | (mik$stock=="Windowpane flounder - Gulf of Maine / Georges Bank_USA-NMFS-5YZ_full" & mik$year < 2015),]

# [41] "Windowpane - Southern New England / Mid-Atlantic-NE"
dd_s41 <- olf[olf$stock=="Windowpane - Southern New England / Mid-Atlantic-NE",]
dd_s41$ts_stock <- "Windowpane Southern New England-Mid Atlantic"
dd_s41$stock <- "Windowpane Southern New England-Mid Atlantic_USA-NMFS-SNEMATL_full"
dd_s41 <- dd_s41[dd_s41$Time > 2014,]
dd_s41$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Windowpane Southern New England-Mid Atlantic_USA-NMFS-SNEMATL_full" | (mik$stock=="Windowpane Southern New England-Mid Atlantic_USA-NMFS-SNEMATL_full" & mik$year < 2015),]

# [42] "Winter flounder - Georges Bank-NE"         
dd_s42 <- olf[olf$stock=="Winter flounder - Georges Bank-NE",]
dd_s42$ts_stock <- "Winter Flounder NAFO-5Z"
dd_s42$stock <- "Winter Flounder NAFO-5Z_USA-NMFS-5Z_full"
dd_s42 <- dd_s42[dd_s42$Time > 2014,]
dd_s42$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Winter Flounder NAFO-5Z_USA-NMFS-5Z_full" | (mik$stock=="Winter Flounder NAFO-5Z_USA-NMFS-5Z_full" & mik$year < 2015),]

# [43] "Winter flounder - Gulf of Maine-NE"      
dd_s43 <- olf[olf$stock=="Winter flounder - Gulf of Maine-NE",]
dd_s43$ts_stock <- "Winter flounder Gulf of Maine"
dd_s43$stock <- "Winter flounder Gulf of Maine"
dd_s43$DemersPelag <- "demersal"

# [44] "Winter flounder - Southern New England / Mid-Atlantic-NE"   
dd_s44 <- olf[olf$stock=="Winter flounder - Southern New England / Mid-Atlantic-NE",]
dd_s44$ts_stock <- "Winter Flounder Southern New England-Mid Atlantic"
dd_s44$stock <- "Winter Flounder Southern New England-Mid Atlantic_USA-NMFS-SNEMATL_full"
dd_s44 <- dd_s44[dd_s44$Time > 2014,]
dd_s44$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Winter Flounder Southern New England-Mid Atlantic_USA-NMFS-SNEMATL_full" | (mik$stock=="Winter Flounder Southern New England-Mid Atlantic_USA-NMFS-SNEMATL_full" & mik$year < 2015),]

# [45] "Witch flounder - Northwestern Atlantic Coast-NE"
dd_s45 <- olf[olf$stock=="Witch flounder - Northwestern Atlantic Coast-NE",]
dd_s45$ts_stock <- "Witch Flounder NAFO-5Y"
dd_s45$stock <- "Witch Flounder NAFO-5Y_USA-NMFS-5Y_full"
dd_s45 <- dd_s45[dd_s45$Time > 2014,]
dd_s45$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Witch Flounder NAFO-5Y_USA-NMFS-5Y_full" | (mik$stock=="Witch Flounder NAFO-5Y_USA-NMFS-5Y_full" & mik$year < 2015),]

# [46] "Yellowtail flounder - Cape Cod / Gulf of Maine-NE"      
dd_s46 <- olf[olf$stock=="Yellowtail flounder - Cape Cod / Gulf of Maine-NE",]
dd_s46$ts_stock <- "Yellowtail flounder Cape Cod / Gulf of Maine"
dd_s46$stock <- "Yellowtail flounder Cape Cod / Gulf of Maine_USA-NMFS-CCOD5Y_full"
dd_s46 <- dd_s46[dd_s46$Time > 2014,]
dd_s46$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Yellowtail flounder Cape Cod / Gulf of Maine_USA-NMFS-CCOD5Y_full" | (mik$stock=="Yellowtail flounder Cape Cod / Gulf of Maine_USA-NMFS-CCOD5Y_full" & mik$year < 2015),]

# [47] "Yellowtail flounder - Georges Bank-NE"   
dd_s47 <- olf[olf$stock=="Yellowtail flounder - Georges Bank-NE",]
dd_s47$ts_stock <- "Yellowtail flounder Georges Bank"
dd_s47$stock <- "Yellowtail flounder Georges Bank_USA-NMFS-5Z_sub"
dd_s47 <- dd_s47[dd_s47$Time > 2013,]
dd_s47$DemersPelag <- "demersal"

# [48] "Yellowtail flounder - Southern New England / Mid-Atlantic-NE"
dd_s48 <- olf[olf$stock=="Yellowtail flounder - Southern New England / Mid-Atlantic-NE",]
dd_s48$ts_stock <- "Yellowtail Flounder Southern New England-Mid Atlantic"
dd_s48$stock <- "Yellowtail Flounder Southern New England-Mid Atlantic_USA-NMFS-SNEMATL_full"
dd_s48 <- dd_s48[dd_s48$Time > 2014,]
dd_s48$DemersPelag <- "demersal"

mik <- mik[mik$stock != "Yellowtail Flounder Southern New England-Mid Atlantic_USA-NMFS-SNEMATL_full" | (mik$stock=="Yellowtail Flounder Southern New England-Mid Atlantic_USA-NMFS-SNEMATL_full" & mik$year < 2015),]

############################################################################################################################################

dd_us_ne <- rbind(dd_s1, dd_s2, dd_s4, dd_s5, dd_s6, dd_s7, dd_s8, dd_s9, dd_s13_14, dd_s15, dd_s16, dd_s17, dd_s18, dd_s19, dd_s20,
                  dd_s21, dd_s22, dd_s23, dd_s24, dd_s25, dd_s26, dd_s27, dd_s28, dd_s29, dd_s32, dd_s33, dd_s36_37, dd_s39, dd_s40,
                  dd_s41, dd_s42, dd_s43, dd_s44, dd_s45, dd_s46, dd_s47, dd_s48)

dd_us_ne <- dd_us_ne[,c(7,1:6,8)]
colnames(dd_us_ne) <- c("ts_stock", "region", "stock", "year", "catch", "TAC", "catch_by_TAC", "DemersPelag")
dd_us_ne$region <- "US northeast"

length(unique(dd_us_ne$stock))  # 37 stocks
dim(dd_us_ne)  # 261 rows

length(unique(mik$stock))  # 470 stocks
dim(mik)  # 11241

length(unique(c(mik$stock, dd_us_ne$stock)))  # 485 stocks
unique(dd_us_ne[!(dd_us_ne$stock %in% mik$stock),]$stock)  # 15 stocks

############################################################################################################################################

## Format Mike's data ##

# Suggested omit
table(mik$suggested_omit)
dd0 <- mik[mik$suggested_omit!="1=missing value",]
length(unique(dd0$stock))  # 413 stocks
length(unique(c(dd0$stock, dd_us_ne$stock)))  # 428 stocks
dim(dd0)  # 7551 rows
dd0 <- dd0[dd0$suggested_omit!="5=smallinteger",]
length(unique(dd0$stock))  # 413 stocks
length(unique(c(dd0$stock, dd_us_ne$stock)))  # 428 stocks
dim(dd0)  # 7540 rows

# Delete TAC = 0 (indicating fishing is closed)
dim(dd0[dd0$TAC == 0,])  # 227 rows 
ccc <- as.data.frame(table(dd0[dd0$TAC == 0,]$ts_stock))
ccc[ccc$Freq > 0,]
dd1 <- dd0[dd0$TAC > 0,]  # 227 rows were deleted
length(unique(dd1$stock))  # 413 stocks
length(unique(c(dd1$stock, dd_us_ne$stock)))  # 428 stocks
table(dd1$suggested_omit)
dim(dd1)  # 7313 rows

# Choose columns
dd <- dd1[,c(1,2,15,5,8,9,11)]
colnames(dd) <- c("region", "ts_stock", "stock", "year", "catch", "TAC", "catch_by_TAC")

# Add life_histories
lh <- read.csv(paste(datadir, "snapp_life_histories.csv", sep="/"), as.is=T)
lh <- lh[,c(2,10)]
dd_lh <- merge(dd, lh, by="ts_stock", all.x=TRUE)
dd_lh$source <- "Mike's data"

# Combine data
dd_us_ne$source <- "NOAA-EDAB"
dat <- rbind(dd_lh, dd_us_ne)
length(unique(dat$stock))  # 428 stocks
dim(dat)  # 7574 rows

table(dat$source)
# Mike's data   NOAA-EDAB 
#        7313         261

# Save olfa 
write.csv(dat, paste(outputdir, "0_mik_olf_data.csv", sep="/"), row.names=F)


