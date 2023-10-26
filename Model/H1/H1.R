# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Packages
library(chron)
library(RColorBrewer)
library(lattice)
library(INLA)
library(splancs)
library(maps)
library(spam)
library(fields)
library(mapdata)
library(gridExtra)
library(rgdal)
library(rgeos)
library(sp)
library(viridisLite)

#####################################################################################################

# Read data
dat <- read.csv("1_dat_for_model.csv", as.is=T)
dat$delpel_code <- as.numeric(factor(dat$DemersPelag, levels=c("bathydemersal", "bathypelagic", "benthic", "benthopelagic", "demersal", 
                                                               "pelagic", "pelagic-neritic", "pelagic-oceanic", "reef-associated")))

# centerizd year
dat$year_code <- dat$year-min(dat$year)+1

# data details
dim(dat)   # 7140
summary(dat$catch_by_TAC)
dim(dat[dat$catch_by_TAC>1,])[1]/dim(dat)[1]  # about 24.44% with catch/TAC>1

# format data for model
dd <- dat[,c(12, 11, 17, 4, 14:16, 18, 7, 19)]

# Y=Catch/TAC (whetehr the mean is different from 1) follows a log-normal distribution
# hypothesis: it will be common to have mean > 1

#################################################################################################################

# Covariates
y <- log(dd$catch_by_TAC+0.1)
year <- dd$year_code
region <- dd$council_code
habitat <- dd$delpel_code
stock <- dd$stock_code
tac <- inla.group(dd$tac_change, 500, method = 'cut')
tac_group1 <- dd$tac_group1
tac_group2 <- dd$tac_group2

#################################################################################################################

# INLA data
ldat = list(Y = y,
            mu = rep(1, length(y)),
            region = region,
            habitat = habitat,
            stock = stock,
            year = year,
            tac = tac,
            tac_group1= tac_group1,
            tac_group2 = tac_group2)


# INLA formula 
form <- Y ~ 0 + mu +
  f(year, replicate=stock, model='rw1', hyper = list(theta = list(prior="pc.prec", param=c(0.5,0.01))), scale.model=TRUE, constr=TRUE)


# call INLA 
res <- inla(form, data=ldat,
            family=c('normal'),
            control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE), 
            control.inla=list(strategy='gaussian',int.strategy='eb'), 
            control.predictor=list(compute=TRUE),
            num.threads=4, keep=TRUE)

res$dic$dic  
res$waic$waic 

