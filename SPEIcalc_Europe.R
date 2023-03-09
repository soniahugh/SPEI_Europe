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

setwd("E:\\Big_Tree\\data\\Europe\\spei\\")

#Functions
f.spei_lud = function(RAIN,TMP, LAT){
  PET=thornthwaite(TMP,LAT,na.rm=TRUE)
  spei.temp= spei(RAIN-PET, scale=3,na.rm=TRUE) #add ref.start
  spei.final=as.vector(spei.temp[[3]])
  return(spei.final)}

months = c('Jan','Feb','Mar','Apr',
           'May','Jun','Jul','Aug',
           'Sep','Oct','Nov','Dec')

gez_zones <- c( 'BorealConiferousForest',
                'BorealMountainSystem', 
                'TemperateContinentalForest1',
                'TemperateContinentalForest2',
                'TemperateMountainSystem',
                'TemperateOceanicForest',
                'SubtropicalMountainSystem',
                'SubtropicalDryForest')
# zone<-'SubtropicalDryForest'

gezSpei<-function(zone){ 
  #Import initial GEZ
  precipRaw<-stack(paste('E:\\Big_Tree\\data\\Europe\\spei\\ERA5_total_precip_monthly_2000_2020_', zone, '.tif', sep = ''))
  tempRaw<-stack(paste('E:\\Big_Tree\\data\\Europe\\spei\\ERA5_mean_temperature_monthly_2000_2020_', zone, '.tif', sep = ''))
  
  #Need to process the data to as the raw precip is in m and the raw temp is in Kelvin.  They need to be in mm and C.
  precip<-stack(precipRaw*1000)
  temp<- stack(tempRaw-273.15)
  
  names(temp) = c(paste0(rep(months,20),'_', rep(2000:2019,each=12)),
                  paste0(months[1:6],'_2020'))
  names(precip) = c(paste0(rep(months,20),'_',rep(2000:2019,each=12)),
                  paste0(months[1:6],'_2020'))
  
  #Calculate SPEI
  LAT = as.vector(coordinates(precip)[,2])
  
  spei.all = list()
  
  for(i in 1:nlayers(precip)){  
    
    print(i)
    spei.all[[i]] = f.spei_lud(as.vector(precip[[i]]),
                               as.vector(temp[[i]]),
                               LAT=LAT[i])
  }
  # A list of dataframes, change back to rasterObject
  spei_spatial = brick(nrow=nrow(precip),
                       ncol = ncol(precip),
                       xmn=extent(precip)[1],
                       xmx=extent(precip)[2],
                       ymn=extent(precip)[3],
                       ymx=extent(precip)[4],nl=dim(precip)[3])
  for(i in 1:dim(precip)[3]){
    spei_spatial[[i]][] = spei.all[[i]]
  }
  
  names(spei_spatial) = c(paste0(rep(months,20),'_',rep(2000:2019,each=12)),
                          paste0(months[1:6],'_2020'))
  
  namesX = c(paste0(rep(months,20),'_',rep(2000:2019,each=12)),
             paste0(months[1:6],'_2020'))
  #Write SPEI Time series to file
  writeRaster(spei_spatial,paste("spei\\SPEI_Jan2000-June2020_", zone, ".tif", sep=''),
              options=c("COMPRESS=LZW", "TFW=NO"),
              format="GTiff",overwrite = TRUE)
  
  ##Calculate the mean of the SPEI time series
  spei_spatial<- stack(spei_spatial)
  # spei_spatial = stack(paste("SPEI_Jan2000-June2020_", zone, ".tif", sep=''))
  names(spei_spatial) = namesX
  
  monthSPEI<-stackApply(spei_spatial, indices = c(rep(1:12,20), rep(1:6,1)), fun = mean)
  names(monthSPEI)<- months
  
  #rasterBrick mean values for each month over the time series
  writeRaster(monthSPEI,
              paste("monthlySpei\\monthlySPEI_Jan2000-June2020_", zone, ".tif", sep=''),
              format = "GTiff",
              options=c("COMPRESS=LZW", "TFW=YES"),
              overwrite=TRUE)
  
  #Find min value, will choose the most dryest value from the monthly series
  
  monthSPEI_min<-min(monthSPEI, na.rm = T)
  
  writeRaster(monthSPEI_min,
              paste("monthlySpeiMin\\monthlySPEI_min_Jan2000-June2020_", zone, ".tif", sep=''),
              format = "GTiff",
              options=c("COMPRESS=LZW", "TFW=YES"),
              overwrite=TRUE)
  
  return(monthSPEI_min)
}

lapply(gez_zones, gezSpei)
