# Define directories
datadir <- "~/Desktop/MSE/data_preparation/Data"
outputdir <- "~/Desktop/MSE/data_preparation/Output"

# Read data
dat <- read.csv(paste(outputdir, "0_mik_olf_data.csv", sep="/"), as.is=T)

length(unique(dat$stock))  # 428 stocks
dim(dat)  # 7574 rows

table(dat$source)
# Mike's data   NOAA-EDAB 
#        7313         261


# Define US councils
dat[dat$region=="US West",]$region <- "US west"
dat$council <- as.character(dat$region)

# us alaska
unique(dat[dat$region=="US Alaska",]$stock)
dat[dat$region=="US Alaska",]$council <- "NPFMC"
dat[dat$stock=="Pacific halibut North Pacific__full",]$council <- "IPHC"

# us northeast
unique(dat[dat$region=="US northeast",]$stock)
dat[dat$region=="US northeast",]$council <- "NEFMC"
dat[dat$stock=="Atlantic butterfish Gulf of Maine / Cape Hatteras_USA-NMFS-NWATLC_full1",]$council <- "MAFMC"
dat[dat$stock=="Atlantic menhaden Atlantic_USA-NMFS-ATL_full",]$council <- "ASMFC"
dat[dat$stock=="Atlantic surfclam Mid-Atlantic Coast_USA-NMFS-MATLC_full",]$council <- "MAFMC"
dat[dat$stock=="Black sea bass Mid-Atlantic Coast_USA-NMFS-MATLC_full",]$council <- "MAFMC"
dat[dat$stock=="Bluefish Atlantic Coast_USA-NMFS-ATLC_full1",]$council <- "MAFMC"
dat[dat$stock=="Longfin inshore squid Atlantic Coast_USA-NMFS-ATLC_full",]$council <- "MAFMC"
dat[dat$stock=="Scup Atlantic Coast_USA-NMFS-ATLC_full1",]$council <- "MAFMC"
dat[dat$stock=="Scup Atlantic Coast_USA-NMFS-ATLC_full2",]$council <- "MAFMC"
dat[dat$stock=="Spiny dogfish Atlantic Coast__full",]$council <- "MAFMC"
dat[dat$stock=="Striped bass Gulf of Maine / Cape Hatteras_USA-NMFS-NWATLC_full",]$council <- "MAFMC"
dat[dat$stock=="Summer flounder Mid-Atlantic Coast_USA-NMFS-MATLC_full",]$council <- "MAFMC"
dat[dat$stock=="Tilefish Mid-Atlantic Coast_USA-NMFS-MATLC_full",]$council <- "MAFMC"
dat[dat$stock=="Atlantic Mackerel Mid-Atlantic Bight",]$council <- "MAFMC"
dat[dat$stock=="Blueline tilefish Mid-Atlantic Bight",]$council <- "MAFMC"
dat[dat$stock=="Chub mackerel Mid-Atlantic Bight",]$council <- "MAFMC"
dat[dat$stock=="Illex squid Mid-Atlantic Bight",]$council <- "MAFMC"
dat[dat$stock=="Ocean quahog Mid-Atlantic Bight",]$council <- "MAFMC"

# us southeast
unique(dat[dat$region=="US southeast",]$stock)
dat[dat$region=="US southeast",]$council <- "GMFMC"
dat[dat$stock=="Black Grouper Gulf of Mexico and South Atlantic_USA-NMFS-GMSATL_full",]$council <- "SAFMC"
dat[dat$stock=="Black sea bass South Atlantic_USA-NMFS-SATL_full",]$council <- "SAFMC"
dat[dat$stock=="Blacknose shark Atlantic_USA-NMFS-ATL_full",]$council <- "AOHS"    # Atlantic HMS
dat[dat$stock=="Blacktip shark Gulf of Mexico_USA-NMFS-GM_full",]$council <- "AOHS"
dat[dat$stock=="Gag Southern Atlantic coast_USA-NMFS-SATLC_full",]$council <- "SAFMC"
dat[dat$stock=="Greater amberjack Southern Atlantic coast_USA-NMFS-SATLC_full",]$council <- "SAFMC"
dat[dat$stock=="King mackerel Southern Atlantic Coast_USA-NMFS-SATLC_full",]$council <- "SAFMC"
dat[dat$stock=="Mutton snapper Southern Atlantic coast and Gulf of Mexico_USA-NMFS-SATLCGM_full",]$council <- "SAFMC"
dat[dat$stock=="Red grouper South Atlantic_USA-NMFS-SATL_full",]$council <- "SAFMC"
dat[dat$stock=="Red porgy Southern Atlantic coast_USA-NMFS-SATLC_full",]$council <- "SAFMC"
dat[dat$stock=="Red snapper Southern Atlantic coast_USA-NMFS-SATLC_full",]$council <- "SAFMC"
dat[dat$stock=="Sandbar shark Atlantic_USA-NMFS-ATL_full",]$council <- "AOHS"
dat[dat$stock=="Snowy grouper Southern Atlantic coast_USA-NMFS-SATLC_sub1",]$council <- "SAFMC"
dat[dat$stock=="Snowy grouper Southern Atlantic coast_USA-NMFS-SATLC_sub2",]$council <- "SAFMC"
dat[dat$stock=="Spanish mackerel Southern Atlantic Coast_USA-NMFS-SATLC_full2",]$council <- "SAFMC"
dat[dat$stock=="Tilefish Southern Atlantic coast_USA-NMFS-SATLC_full",]$council <- "SAFMC"
dat[dat$stock=="Vermilion snapper Southern Atlantic coast_USA-NMFS-SATLC_full",]$council <- "SAFMC"
dat[dat$stock=="Yellowtail Snapper Southern Atlantic Coast and Gulf of Mexico_USA-NMFS-SATLCGM_full",]$council <- "SAFMC"

# us west
unique(dat[dat$region=="US west",]$stock)
dat[dat$region=="US west",]$council <- "PFMC"
dat[dat$stock=="Arrowtooth flounder Pacific Coast__full",]$council <- "NPFMC"
dat[dat$stock=="Pacific hake Pacific Coast__full",]$council <- "US non-federal"
dat[dat$stock=="Pacific hake Pacific Coast_US_sub",]$council <- "US non-federal"

# high seas
unique(dat[dat$region=="high seas",]$stock)
dat[dat$stock=="Albacore tuna North Atlantic__full",]$council <- "AOHS"
dat[dat$stock=="Albacore tuna South Atlantic__full",]$council <- "AOHS"
dat[dat$stock=="Bigeye tuna Atlantic__full",]$council <- "AOHS"
dat[dat$stock=="Blue marlin Atlantic__full",]$council <- "AOHS"
dat[dat$stock=="Bluefin tuna Eastern Atlantic__full",]$council <- "AOHS"
dat[dat$stock=="Bluefin tuna Western Atlantic__full",]$council <- "AOHS"
dat[dat$stock=="Pacific bluefin tuna Pacific Ocean_IATTC_full",]$council <- "POHS"
dat[dat$stock=="Southern bluefin tuna Southern Oceans__full",]$council <- "SOs"
dat[dat$stock=="Swordfish North Atlantic__full",]$council <- "AOHS"
dat[dat$stock=="Swordfish South Atlantic__",]$council <- "AOHS"
dat[dat$stock=="White marlin Atlantic__full",]$council <- "AOHS"
dat[dat$stock=="Yellowfin tuna Atlantic__full",]$council <- "AOHS"

# council code
dat$council_code <- as.numeric(as.factor(dat$council))


# delete stocks with only one record
check <- as.data.frame(table(dat$stock))
keep_stocks <- check[check$Freq>1,]
length(unique(keep_stocks$Var1))  # 426 stocks
dd1 <- dat[dat$stock %in% unique(keep_stocks$Var1),]
dd1$stock_code <- as.numeric(as.factor(dd1$stock))
length(unique(dd1$stock))  # 426 stocks
dim(dd1)  # 7572 rows


# TAC change
# two groups: 1-increased or unchanged from previous year, 2-decreased 
# three groups: 1-increase by more than 10%, 2-decrease by more than 10%, 3-stay within -10-10% 
# Y=Catch/TAC (whetehr the mean is different from 1) follows a log-normal distribution
# hypothesis: it will be common to have mean > 1
dd1$tac_change <- dd1$tac_group2 <- dd1$tac_group1 <- dd1$year_rank <- NA
for (i in 1:length(unique(dd1$stock_code))){
  dd1[dd1$stock_code==i,]$year_rank <- rank(dd1[dd1$stock_code==i,]$year)
  for (j in 2:max(dd1[dd1$stock_code==i,]$year_rank)){
    dd1[dd1$stock_code==i & dd1$year_rank==j,]$tac_group1 <- ifelse(length(dd1[dd1$stock_code==i & dd1$year_rank==j,]$TAC-dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC)==0, NA, 
                                                                    ifelse(dd1[dd1$stock_code==i & dd1$year_rank==j,]$TAC-dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC >= 0, 1, 2))
    
    dd1[dd1$stock_code==i & dd1$year_rank==j,]$tac_group2 <- ifelse(length(dd1[dd1$stock_code==i & dd1$year_rank==j,]$TAC-dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC)==0, NA, 
                                                                    ifelse(dd1[dd1$stock_code==i & dd1$year_rank==j,]$TAC > 1.1*dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC, 1,
                                                                           ifelse(dd1[dd1$stock_code==i & dd1$year_rank==j,]$TAC < 0.9*dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC, 2,
                                                                                  ifelse(dd1[dd1$stock_code==i & dd1$year_rank==j,]$TAC >= 0.9*dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC &
                                                                                           dd1[dd1$stock_code==i & dd1$year_rank==j,]$TAC <= 1.1*dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC, 3))))
    
    dd1[dd1$stock_code==i & dd1$year_rank==j,]$tac_change <- ifelse(length(dd1[dd1$stock_code==i & dd1$year_rank==j,]$TAC-dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC)==0, NA, 
                                                                    (dd1[dd1$stock_code==i & dd1$year_rank==j,]$TAC-dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC)/dd1[dd1$stock_code==i & dd1$year_rank==j-1,]$TAC)
  }
}


# Check NA's
summary(dd1)  # 426 NA's on tac_change (first year)
dd2 <- dd1[!is.na(dd1$tac_change),]
length(unique(dd2$stock))  # 426 stocks
dim(dd2)  # 7146 rows


# Delete stocks with only one record
check <- as.data.frame(table(dd2$stock))
keep_stocks <- check[check$Freq>1,]
length(unique(keep_stocks$Var1))  # 420 stocks
dd3 <- dd2[dd2$stock %in% unique(keep_stocks$Var1),]
dd3$stock_code <- as.numeric(as.factor(dd3$stock))
length(unique(dd3$stock))  # 420 stocks
dim(dd3)  # 7140

# format region-specific stock key
dd3$rs_key_code <- NA
for (i in 1:24){
  dd3[dd3$council_code==i,]$rs_key_code <- as.numeric(as.factor(dd3[dd3$council_code==i,]$stock))
}


# Catch/TAC
dd <- dd3
dim(dd[dd$catch_by_TAC==0,])  # 23 row 
table(dd[dd$catch_by_TAC==0,]$stock)  # 12 stocks
dd[dd$catch_by_TAC==0 & dd$catch>0,]$catch
dd[dd$catch_by_TAC==0 & dd$catch>0,]$TAC
dd[dd$catch_by_TAC==0 & dd$catch>0,]$catch_by_TAC <- dd[dd$catch_by_TAC==0 & dd$catch>0,]$catch/dd[dd$catch_by_TAC==0 & dd$catch>0,]$TAC
dd[dd$catch_by_TAC==0,]$catch_by_TAC <- (dd[dd$catch_by_TAC==0,]$catch+0.1)/dd[dd$catch_by_TAC==0,]$TAC

summary(dd)

## save data from models
write.csv(dd, paste(outputdir, "1_dat_for_model.csv", sep="/"), row.names=F)


