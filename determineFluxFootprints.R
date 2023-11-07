library('neonUtilities')
library('neonOS')
library('tidyverse')
library('raster')
library('rasterVis')
library('viridis')

#Define folder names ----
fluxSaveFolder <- "NEON_EddyCov_Data/"
fluxFootprintFolder <- 'fluxFootprintData/'
dir.create(fluxFootprintFolder)

#Site Information ----
allSiteData <- read.csv('NEON_Field_Site_Metadata_20230309.csv') %>% 
  filter(field_site_type %in% c("Gradient Terrestrial","Core Terrestrial")) #Downloaded from https://www.neonscience.org/field-sites/explore-field-sites
availableFluxYears <- read.csv('availableFluxYears_condensed.csv')
phenoDates <- read.csv('summaryPhenocamTransitions.csv')

#Define Function To Extract Date from raster name ----
extractDte <- function(X){
  yr <- strsplit(X,"[.]")[[1]][23]
  mth <- strsplit(X,"[.]")[[1]][24]
  dy <- strsplit(X,"[.]")[[1]][25]
  return(paste(yr,mth,dy,sep="-"))
}

#For each site-year unzip NEON data, stack all rasters, and calculate means for each desired period (full year, spring greenup, fall brown down, and main growing season) based off of PhenoCam timings
for(i in 1:nrow(availableFluxYears)){
  siteName=availableFluxYears$field_site_id[i]
  year=availableFluxYears$year[i]
  print(siteName)
  print(year)
  
  siteYearPheno <- phenoDates %>% filter(siteID==siteName & year==availableFluxYears$year[i])
  files <- paste0(fluxSaveFolder,siteName,'/filesToStack00200/',intersect(dir(path=paste0(fluxSaveFolder,siteName,'/filesToStack00200'),pattern=".zip"),
                                                                          dir(path=paste0(fluxSaveFolder,siteName,'/filesToStack00200'),pattern=paste0('DP4.00200.001.',year))))
  allFoot <- lapply(files,function(X){
    return (footRaster(filepath=X))
  })

  allFoot = stack(unlist(allFoot))

  footNames <- names(allFoot)
  footNames <- footNames[2:length(footNames)] #Later adding 1 to indices because of removed first one (which is the mean of the stack in each file)
  footDates <- as.Date(as.character(lapply(footNames,extractDte)))
  
  annualMean <- calc(allFoot,fun=mean,na.rm=TRUE)
  writeRaster(annualMean,filename=paste0(fluxFootprintFolder,siteName,"_",year,"_annualMeanFootprint.tif"),
              format="GTiff",overwrite=TRUE)
  rm(annualMean) #Need to remove objects to keep RAM down
  
  #cellStats(annualMean,sum) #Check to make sure equals 1
  
  growingSeasonFoot <- subset(allFoot,(which(footDates>siteYearPheno$risingEnd & footDates<siteYearPheno$fallingStart)+1))
  growingSeasonMean <- calc(growingSeasonFoot,fun=mean,na.rm=TRUE)
  
  writeRaster(growingSeasonMean,filename=paste0(fluxFootprintFolder,siteName,"_",year,"_growingSeasonMeanFootprint.tif"),
              format="GTiff",overwrite=TRUE)
  rm(growingSeasonMean,growingSeasonFoot) #Need to remove objects to keep RAM down
  
  risingFoot <- subset(allFoot,(which(footDates>siteYearPheno$risingStart & footDates<siteYearPheno$risingEnd)+1))
  risingMean <- calc(risingFoot,fun=mean,na.rm=TRUE)
  
  writeRaster(risingMean,filename=paste0(fluxFootprintFolder,siteName,"_",year,"_risingMeanFootprint.tif"),
              format="GTiff",overwrite=TRUE)
  rm(risingMean,risingFoot) #Need to remove objects to keep RAM down
  
  fallingFoot <- subset(allFoot,(which(footDates>siteYearPheno$fallingStart & footDates<siteYearPheno$fallingEnd)+1))
  fallingMean <- calc(fallingFoot,fun=mean,na.rm=TRUE)
  
  writeRaster(fallingMean,filename=paste0(fluxFootprintFolder,siteName,"_",year,"_fallingMeanFootprint.tif"),
              format="GTiff",overwrite=TRUE)
  rm(fallingMean,fallingFoot) #Need to remove objects to keep RAM down
}
