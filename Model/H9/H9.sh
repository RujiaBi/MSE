#!/bin/bash

#PBS -l nodes=1:ppn=5,pmem=50gb
#PBS -l walltime=144:00:00
#PBS -q normal_q
#PBS -A BiSpatial17

# Add modules 
module reset
module load GEOS/3.9.1-GCC-10.3.0
module load gcc/8.2.0
module load GDAL/3.3.0-foss-2021a
module load PROJ/8.0.1-GCCcore-10.3.0
module load openblas
module load R/4.0.2-foss-2020a
export R_LIBS=/home/rbi/R_libs_TK
  
Rscript H9.R
exit;
