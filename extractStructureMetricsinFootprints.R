# R script for extracting structure info from lidar data.
#Note, reads in data from "points" output from determineFluxFootprints.R

library('neonUtilities')
library('neonOS')
library('lidR')
#devtools::install_github("akamoske/canopyLazR")
library('canopyLazR')
library('sf')

####Function to convert from lat-long to UTM
LongLatToUTM<-function(lon,lat,zone){
  xy <- data.frame(ID = 1:length(lon), X = lon, Y = lat)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

#######NOTE: For the next 2 steps crosscheck zone and epsg codes for other NEON sites
#Create a polygon from the 3 coordinates from points dataframe generated from determineFluxFootprints.R
poly<-points%>%st_as_sf(coords=c("lon", "lat"), crs=4326)%>%
  st_transform(crs = "EPSG:32617")%>% # Has the same crs as the lidar file which is in UTM
  st_combine()%>%st_cast("POLYGON")
plot(poly)


#Converting fluxtower coordinates to UTM
utmcor<-LongLatToUTM(points$lon, points$lat, zone=17)

#Read in Neon AOP data
dpID <- "DP1.30003.001"
siteID <- "MLBS"
eastings<- c(min(utmcor$X), min(utmcor$X),max(utmcor$X),max(utmcor$X)) #to get the entire extent of the footprint
northings<-c(min(utmcor$Y), max(utmcor$Y),min(utmcor$Y), max(utmcor$Y)) #to get the entire extent of the footprint
byTileAOP(dpID=dpID,
          site=siteID,
          year="2021",
          easting=eastings,
          northing=northings,
          check.size = FALSE) 


#Lidar data as a LASCatalog (all n files are loaded)
ctg<-readLAScatalog("DP1.30003.001/neon-aop-products/2021/FullSite/D07/2021_MLBS_4/L1/DiscreteLidar/ClassifiedPointCloud/")
laz_footprint<-clip_roi(ctg, poly) #clipping the .las file to the flux footprint area
laz_footprint_filter<-filter_poi(laz_footprint,Z>1000, Z<1300 ) #filtering really high and low values; for other sites we may want to consider some outlier analysis?
writeLAS(laz_footprint_filter, paste0("lidar_footprint_", siteID,".laz"))

#Code modified from SPECschool resources (https://github.com/kdahlin/SPEC_School/blob/main/StationG/lidarData_KW.R)
# Convert .laz or .las file into a voxelized lidar array
laz.data <- laz.to.array("lidar_footprint_MLBS.laz", 
                         voxel.resolution = 10, 
                         z.resolution = 1,
                         use.classified.returns = TRUE)


# Level the voxelized array to mimic a canopy height model
level.canopy <- canopy.height.levelr(lidar.array = laz.data)

# Estimate LAD for each voxel in leveled array
lad.estimates <- machorn.lad(leveld.lidar.array = level.canopy, 
                             voxel.height = 1, 
                             beer.lambert.constant = NULL)

#Structure metrics
epsg_codes<-32617
# Calculate the ratio of filled and empty voxels in a given column of the canopy
empty.filled.ratio <- canopy.porosity.filled.ratio(lad.array = lad.estimates,
                                                   laz.array = laz.data,
                                                   ht.cut = 5,
                                                   epsg.code = epsg_codes)

# Calculate the volume of filled and empty voxles in a given column of the canopy
empty.filled.volume <- canopy.porosity.filled.volume(lad.array = lad.estimates,
                                                     laz.array = laz.data,
                                                     ht.cut = 5,
                                                     xy.res = 10,
                                                     z.res = 1,
                                                     epsg.code = epsg_codes)

# Calculate the within canopy rugosity
within.can.rugosity <- rugosity.within.canopy(lad.array = lad.estimates,
                                              laz.array = laz.data,
                                              ht.cut = 5,
                                              epsg.code = epsg_codes)
plot(within.can.rugosity$rugosity.raster)
plot(poly, add=T)

# Calculate various canopy volume metrics from Lefsky
can.volume <- canopy.volume(lad.array = lad.estimates,
                            laz.array = laz.data,
                            ht.cut = 5,
                            xy.res = 10,
                            z.res = 1,
                            epsg.code = epsg_codes)

# We can calculate the depth of the euphotic zone by dividing by the volume of the voxel
euphotic.depth <- can.volume$euphotic.volume.column.raster / ( 10 * 10 * 1)





