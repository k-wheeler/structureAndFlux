library('neonUtilities')
library('neonOS')
library(sp)
#options(timeout=600)

#Site Information ----
siteName <- "MLBS"
allSiteData <- read.csv('NEON_Field_Site_Metadata_20230309.csv') #Downloaded from https://www.neonscience.org/field-sites/explore-field-sites
towerLocation <- allSiteData[allSiteData$field_site_id==siteName,]
coordinates(towerLocation)=~field_longitude + field_latitude
proj4string(towerLocation)<- CRS("+proj=longlat +datum=WGS84")

siteLat <- allSiteData$field_latitude[allSiteData$field_site_id==siteName]
siteLon <- allSiteData$field_longitude[allSiteData$field_site_id==siteName]

#Download and Stack NEON Data ----
if(file.exists('/Users/Kathryn/Documents/NEON_Token.R')){
  source('/Users/Kathryn/Documents/NEON_Token.R')
  zipsByProduct(dpID="DP4.00200.001",
                site=siteName,
                check.size = F,
                package="basic",
                startdate="2020-07-01",
                enddate="2021-06-31",
                token = NEON_TOKEN)
}else{
  zipsByProduct(dpID="DP4.00200.001",
                site=siteName,
                check.size = F,
                package="basic",
                startdate="2020-07-01",
                enddate="2021-06-31")
}

flux <- stackEddy(filepath='filesToStack00200/',
                  level="dp04")

#Example Calculation of Footprint Coordinates ----
i=1
distYaxs90 <- flux$MLBS$data.foot.stat.distYaxs90[i] #one-sided cross-wind distance of the 90% along-wind integrated cumulative footprint
distXaxs90 <- flux$MLBS$data.foot.stat.distXaxs90[i] #along-wind distance of the 90% crosswind-integrated cumulative footprint
#flux$MLBS$data.foot.stat.distXaxsMax #Along-wind distance of max contribution peak
angZaxsErth <- flux$MLBS$data.foot.stat.angZaxsErth[i] #Wind direction

angZaxsErth_radian <- angZaxsErth %%90 * pi/180 #For trig, need angle less than 90 degrees and need to convert to radians
hypo <- sqrt(distYaxs90**2 + distXaxs90**2)
insideAngle = asin(distYaxs90/hypo)
outsideAngle = angZaxsErth_radian - insideAngle #In Radians
opp_offset = sin(outsideAngle)*hypo
adj_offset = cos(outsideAngle)*hypo

r_earth=6378137 #Earth's radius
points = data.frame(lat=siteLat,lon=siteLon)
for(i in 1:2){
  if(i==1){
    dy=opp_offset
    dx=adj_offset
  }else{
    dx=opp_offset
    dy=adj_offset
  }
  #Determine if you add or substract the offsets for lat/lon based off of initial angle
  if(angZaxsErth> 0 & angZaxsErth<90){
    new_latitude  = lat  + (dy / r_earth) * (180 / pi)
    new_longitude = lon + (dx / r_earth) * (180 / pi) / cos(lat * pi/180)
  }else if(angZaxsErth> 90 & angZaxsErth<180){
    new_latitude  = lat  - (dy / r_earth) * (180 / pi)
    new_longitude = lon + (dx / r_earth) * (180 / pi) / cos(lat * pi/180)
  }else if(angZaxsErth> 180 & angZaxsErth<270){
    new_latitude  = lat  - (dy / r_earth) * (180 / pi)
    new_longitude = lon - (dx / r_earth) * (180 / pi) / cos(lat * pi/180)
  }else if(angZaxsErth> 270 & angZaxsErth<360){
    new_latitude  = lat  + (dy / r_earth) * (180 / pi)
    new_longitude = lon - (dx / r_earth) * (180 / pi) / cos(lat * pi/180)
  }else{
    print("Unknown Angle")
  }
  points <- rbind(points,c(new_latitude,new_longitude))
}
print(points)
