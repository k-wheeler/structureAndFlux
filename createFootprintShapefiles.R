library('neonUtilities')
library('neonOS')
library('tidyverse')
library('raster')
library('rasterVis')
library('viridis')
library('terra')

#Define folder names ----
fluxSaveFolder <- "NEON_EddyCov_Data/"
fluxFootprintFolder <- 'fluxFootprintData/'

#Site Information ----
allSiteData <- read.csv('NEON_Field_Site_Metadata_20230309.csv') %>% 
  filter(field_site_type %in% c("Gradient Terrestrial","Core Terrestrial")) #Downloaded from https://www.neonscience.org/field-sites/explore-field-sites
availableFluxYears <- read.csv('availableFluxYears_condensed.csv')

timePeriods <- c('annual','growingSeason','rising','falling')
#For each site-year unzip NEON data, stack all rasters, and calculate means for each desired period (full year, spring greenup, fall brown down, and main growing season) based off of PhenoCam timings
for(i in 1:nrow(availableFluxYears)){
  siteName=availableFluxYears$field_site_id[i]
  year=availableFluxYears$year[i]
  print(siteName)
  print(year)
  
  lapply(timePeriods,function(X){
    ras <- raster(paste0(fluxFootprintFolder,siteName,"_",year,"_",X,"MeanFootprint.tif"))
    df <- as.data.frame(ras, xy=TRUE)
    rasVals=sort(df$layer,decreasing = TRUE)
    rasVals <- data.frame(v=rasVals,cs=cumsum(rasVals)) %>% filter(!is.na(v))
    cutoff <- rasVals$v[which(rasVals$cs>0.9)[1]] #Want to include the numbers that sum up to covering the area 90% of the time
    
    polRast <- rast(ras >= cutoff) #Separates ras into a false and a true polygon
    pol <- as.polygons(polRast)[2] #Only keep the polygon where values are >= cutoff
    pol <- st_as_sf(pol) #Cast to simple feature
    
    #plot(ras)
    #plot(pol,add=TRUE)
    
    st_write(pol, paste0(fluxFootprintFolder,siteName,"_",year,"_",X,"MeanFootprint_90polygon.shp")) #Write as shape file
    
  })
}
