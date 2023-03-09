###Calculating SPEI###

#Taking the script given from Ludmilla Rattis, calculate SPEI for the European study area per GEZ.

#Import library
library(devtools)
install_github('sbegueria/SPEI',force=TRUE)

library(raster)
library(SPEI)
library(rgdal)
library(rasterVis)
library(tidyverse)
library(gridExtra)
library(R.utils)
library(Rcpp)
library(manipulate)
library(viridisLite)
library(pals)

#Functions
f.spei_lud = function(RAIN,TMP, LAT){
  PET=thornthwaite(TMP,LAT,na.rm=TRUE)
  spei.temp= spei(RAIN-PET, scale=3,na.rm=TRUE) #add ref.start
  spei.final=as.vector(spei.temp[[3]])
  return(spei.final)}

#Import initial GEZ
precip<-stack('E:\\Big_Tree\\data\\Europe\\spei\\ERA5_total_precip_monthly_2000_2020_SubtropicalDryForest.tif')
