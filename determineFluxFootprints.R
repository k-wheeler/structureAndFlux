library('neonUtilities')
library('neonOS')
library('tidyverse')

#Define folder names ----
fluxSaveFolder <- "NEON_EddyCov_Data/"
fluxFootprintFolder <- 'fluxFootprintData'
dir.create(fluxFootprintFolder)

#Site Information ----
allSiteData <- read.csv('NEON_Field_Site_Metadata_20230309.csv') %>% 
  filter(field_site_type %in% c("Gradient Terrestrial","Core Terrestrial")) #Downloaded from https://www.neonscience.org/field-sites/explore-field-sites
availableFluxYears <- read.csv('availableFluxYears.csv')

#Function to calculate coordinates ----
calculateFootprintCoordinates <- function(X,siteID,lat,lon){
  r_earth=6378137 #Earth's radius
  distYaxs90 <- as.numeric(getElement(X,'data.foot.stat.distYaxs90')) #one-sided cross-wind distance of the 90% along-wind integrated cumulative footprint
  distXaxs90 <- as.numeric(getElement(X,'data.foot.stat.distXaxs90')) #along-wind distance of the 90% crosswind-integrated cumulative footprint
  #flux$MLBS$data.foot.stat.distXaxsMax #Along-wind distance of max contribution peak
  angZaxsErth <- as.numeric(getElement(X,'data.foot.stat.angZaxsErth')) #Wind direction
  qf <- getElement(X,'qfqm.foot.turb.qfFinl')
  if(is.na(qf)){
    qf <- 1
  }
  points <- c(siteID,as.character(getElement(X,'timeBgn')),as.character(getElement(X,'timeEnd')),lat,lon)
  if(is.na(distYaxs90) || is.na(distXaxs90) || is.na(angZaxsErth) || (qf!=0)){
    points <- c(points,NA,NA,NA,NA)
  }else{
    angZaxsErth_radian <- angZaxsErth %% 90 * pi/180 #For trig, need angle less than 90 degrees and need to convert to radians
    hypo <- sqrt(distYaxs90**2 + distXaxs90**2)
    insideAngle = asin(distYaxs90/hypo) #Will be the same for both smallerWindDirection triangle and largerWindDirection triangle
    outsideAngle_smallerWindDirection = angZaxsErth_radian - insideAngle #In Radians #Indicates the point that is at a smaller wind direction
    
    #Smaller wind direction coordinate refers to the coordinate that is at a smaller wind direction angle (clockwise from N)
    if(outsideAngle_smallerWindDirection>0){ #Stays inside quadrant (If wind direction is NE, the smaller wind direction coordinate is also NE)
      opp_smallerWindDirection = sin(outsideAngle_smallerWindDirection) * hypo
      adj_smallerWindDirection = cos(outsideAngle_smallerWindDirection) * hypo
    }else if(outsideAngle_smallerWindDirection<0){ #Exits quadrant
      angleOver <- abs(outsideAngle_smallerWindDirection)
      opp_smallerWindDirection = -1*sin(angleOver) * hypo
      adj_smallerWindDirection = cos(angleOver) * hypo
    }else{
      print("Unaccounted for angle in smaller wind direction")
      exit()
    }
    
    outsideAngle_largerWindDirection = pi/2 - 2 * insideAngle - outsideAngle_smallerWindDirection #4 angles add up to pi/2 (90 degrees)
    
    if(secondOutsideAngle>0){ #Larger wind direction coordinate stays in quadrant
      opp_largerWindDirection = sin(outsideAngle_largerWindDirection) * hypo
      adj_largerWindDirection = cos(outsideAngle_largerWindDirection) * hypo
    }else if(secondOutsideAngle<0){ #Exits
      angleOver <- abs(outsideAngle_largerWindDirection)
      opp_largerWindDirection = -1*sin(angleOver) * hypo
      adj_largerWindDirection = cos(angleOver) * hypo
    }else{
      print("Unaccounted for angle in larger wind direction")
      exit()
    }
    
    if(angZaxsErth> 0 & angZaxsErth<90){
      delta_lat_smallerWindDirection = adj_smallerWindDirection
      delta_lon_smallerWindDirection = opp_smallerWindDirection
      
      delta_lat_largerWindDirection = opp_largerWindDirection
      delta_lon_largerWindDirection = adj_largerWindDirection
      
    }else if(angZaxsErth> 90 & angZaxsErth<180){
      delta_lat_smallerWindDirection = -1*opp_smallerWindDirection
      delta_lon_smallerWindDirection = adj_smallerWindDirection
      
      delta_lat_largerWindDirection = -1*adj_largerWindDirection
      delta_lon_largerWindDirection = opp_largerWindDirection
      
    }else if(angZaxsErth> 180 & angZaxsErth<270){
      delta_lat_smallerWindDirection = -1*adj_smallerWindDirection
      delta_lon_smallerWindDirection = -1*opp_smallerWindDirection
      
      delta_lat_largerWindDirection = -1*opp_largerWindDirection
      delta_lon_largerWindDirection = -1*adj_largerWindDirection
      
    }else if(angZaxsErth> 270 & angZaxsErth<360){
      delta_lat_smallerWindDirection = opp_smallerWindDirection
      delta_lon_smallerWindDirection = -1*adj_smallerWindDirection
      
      delta_lat_largerWindDirection = adj_largerWindDirection
      delta_lon_largerWindDirection = -1*opp_largerWindDirection
    }
    
    new_latitude_smallerWindDirection  = lat  + (delta_lat_smallerWindDirection / r_earth) * (180 / pi)
    new_longitude_smallerWindDirection = lon + (delta_lon_smallerWindDirection / r_earth) * (180 / pi) / cos(lat * pi/180)
    
    new_latitude_largerWindDirection  = lat  + (delta_lat_largerWindDirection / r_earth) * (180 / pi)
    new_longitude_largerWindDirection = lon + (delta_lon_largerWindDirection / r_earth) * (180 / pi) / cos(lat * pi/180)
    
    points <- c(points,round(new_latitude_smallerWindDirection,digits=6),round(new_longitude_smallerWindDirection,digits=6),
                round(new_latitude_largerWindDirection,digits=6),round(new_longitude_largerWindDirection,digits=6))
  }
  names(points) <- c('field_site_id','timeBgn','timeEnd','lat1','lon1','lat2','lon2','lat3','lon3')
  return(points)
}

#Stack NEON Data and Calculate Footprint Coordinates (Files downloaded in the downloadFluxData.R script) ----
for(s in 1:nrow(allSiteData)){
  siteName <- allSiteData$field_site_id[s]
  print(siteName)
  siteLat <- allSiteData$field_latitude[s]
  siteLon <- allSiteData$field_longitude[s]
  
  flux <- stackEddy(filepath=paste0(fluxSaveFolder,siteName,'/filesToStack00200'),
                    level="dp04")  #Stack zip files
  
  #Calculate footprints
  siteFootprints=t(apply(flux[1][[1]],MARGIN=1,FUN=calculateFootprintCoordinates,siteID=siteName,lat=siteLat,lon=siteLon))
  
  #Save as site-specific csv
  write.csv(siteFootprints,file=paste0(fluxFootprintFolder,"/",siteName,"_footprintCoordinates.csv"),row.names=FALSE,quote=FALSE)
}

#Combine site-specific files into a comprehensive file of footprints ----
s=1
siteName <- allSiteData$field_site_id[s]
allFootprints <- read.csv(paste0(fluxFootprintFolder,"/",siteName,"_footprintCoordinates.csv"))
for(s in 2:nrow(allSiteData)){
  siteName <- allSiteData$field_site_id[s]
  print(siteName)
  allFootprints <- rbind(allFootprints,read.csv(file=paste0(fluxFootprintFolder,"/",siteName,"_footprintCoordinates.csv")))
}
write.csv(allFootprints,file=paste0(fluxFootprintFolder,"/all_footprintCoordinates.csv"),row.names=FALSE,quote=FALSE)

