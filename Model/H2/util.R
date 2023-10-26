#if(!require('rjags')) install.packages('rjags')
#if(!require('runjags')) install.packages('runjags')
#if(!require('rlecuyer')) install.packages('rlecuyer')
#if(!require('snow')) install.packages('snow')
#if(!require('snowfall')) install.packages('snowfall')
#if(!require('RODBC')) install.packages('RODBC')

library("R2jags")
library('rjags')
library('runjags')
library('rlecuyer')
library('snow')
library('snowfall')

require(parallel)


#install.packages('rjags',
#                 configure.args="--with-jags-libdir=/opt/apps/gcc5_2/openmpi1_10/JAGS/4.2.0/lib/")