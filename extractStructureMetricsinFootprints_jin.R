# R script for extracting structure info from lidar data.
#Note, reads in data from "points" output from determineFluxFootprints.R

library('neonUtilities')
library('neonOS')
library('lidR')
#devtools::install_github("akamoske/canopyLazR")
library('canopyLazR')
library('sf')


options(stringsAsFactors=F)
# set working directory rewrite to wherever you are keeping struct_flux
# 
wd <- path.expand("/Volumes/jins_wd/data/struct_flux")
setwd(wd)
datadir <- file.path(wd, 'Data')

# Set year of analysis
yr <- '2021'
# Set site of interest
site_oi <- 'MLBS'

# Load in site metadata to figure coordinates to load.
siteinfo <- read.csv(file.path(datadir, 'NEON_Field_Site_Metadata_20230309.csv'))
aopinfo <- read.csv(file.path(datadir, 'AOP_NIS_FlightDates_2013_2021.csv'))
footprintinfo <- read.csv(file.path(datadir, 'all_footprintCoordinates.csv'))
footprintsite <- footprintinfo[footprintinfo$field_site_id == site_oi, ]
footprintsite <- na.omit(footprintsite)
# Create the polygon for each footprint
footprintsite$geom_poly = sprintf("POLYGON((%s %s, %s %s, %s %s, %s %s))", 
                             footprintsite$lon1, footprintsite$lat1, 
                             footprintsite$lon2, footprintsite$lat2, 
                             footprintsite$lon3, footprintsite$lat3, 
                             footprintsite$lon1, footprintsite$lat1)

# Create one side of the polygon so we can measure length 1
footprintsite$geom_str1 = sprintf("LINESTRING(%s %s, %s %s)", 
                                  footprintsite$lon1, footprintsite$lat1, 
                                  footprintsite$lon2, footprintsite$lat2)

# Create other side of the polygon so we can measure length 2
footprintsite$geom_str2 = sprintf("LINESTRING(%s %s, %s %s)", 
                                  footprintsite$lon1, footprintsite$lat1, 
                                  footprintsite$lon3, footprintsite$lat3)

# Create different dataframes for polygons and lines.
footprintdf = st_as_sf(footprintsite, wkt="geom_poly", crs=4326)
footprintl1 = st_as_sf(footprintsite, wkt="geom_str1", crs=4326)
footprintl2 = st_as_sf(footprintsite, wkt="geom_str2", crs=4326)


# Get Tower coordinates
lon_site <- siteinfo[siteinfo$field_site_id == site_oi,]$field_longitude
lat_site <- siteinfo[siteinfo$field_site_id == site_oi,]$field_latitude
easting_site <- siteinfo[siteinfo$field_site_id == site_oi,]$field_utm_easting
northing_site <- siteinfo[siteinfo$field_site_id == site_oi,]$field_utm_northing
utm_zone_site <- siteinfo[siteinfo$field_site_id == site_oi,]$field_utm_zone
domain_id_site <- siteinfo[siteinfo$field_site_id == site_oi,]$field_domain_id

# Change WGS84 projection to UTM at specific site
footprintdf <- st_transform(footprintdf, crs = paste("EPSG:326", 
                         sprintf("%02d", 
                                 as.numeric(gsub('N', '', utm_zone_site))), 
                         sep=""))# Has the same crs a


footprintl1 <- st_transform(footprintl1, crs = paste("EPSG:326", 
                                                     sprintf("%02d", 
                                                             as.numeric(gsub('N', '', utm_zone_site))), 
                                                     sep=""))

footprintl2 <- st_transform(footprintl2, crs = paste("EPSG:326", 
                                                     sprintf("%02d", 
                                                             as.numeric(gsub('N', '', utm_zone_site))), 
                                                     sep=""))


# Create the buffersize for extraction of the site.
# Hmm buffer can be larger than tile I guess?
buffersize <- as.numeric(max(st_length(footprintl2$geom_str2), st_length(footprintl1$geom_str1)))

####Function to convert from lat-long to UTM
# LongLatToUTM<-function(lon,lat,zone){
#   xy <- data.frame(ID = 1:length(lon), X = lon, Y = lat)
#   coordinates(xy) <- c("X", "Y")
#   proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
#   res <- spTransform(xy, CRS(paste("+proj=utm +zone=",utm_zone_site," ellps=WGS84",sep='')))
#   return(as.data.frame(res))
# }
#######NOTE: For the next 2 steps crosscheck zone and epsg codes for other NEON sites
## This is the original script
#Create a polygon from the 3 coordinates from points dataframe generated from determineFluxFootprints.R
# poly <- points%>%st_as_sf(coords=c("lon", "lat"), crs=4326)%>%
#   st_transform(crs = "EPSG:32617")%>% # Has the same crs as the lidar file which is in UTM
#   st_combine()%>%st_cast("POLYGON")
# plot(poly)

# https://stackoverflow.com/questions/48383990/convert-sequence-of-longitude-and-latitude-to-polygon-via-sf-in-r
# Create list of coordinates with lon and lats from a single row
# Theoretically we can make polygons for every footprint
# df <- data.frame(
#   lon = as.numeric(footprintinfo[1,c(5, 7, 9)]),
#   lat = as.numeric(footprintinfo[1,c(4, 6, 8)])
# )

# poly <- df %>%
#   st_as_sf(coords=c("lon", "lat"), crs=4326) %>%
#   st_transform(crs = paste("EPSG:326", 
#                            sprintf("%02d", 
#                                    as.numeric(gsub('N', '', utm_zone_site))), 
#                            sep=""))%>% # Has the same crs as the lidar file which is in UTM
#   st_combine()%>%st_cast("POLYGON")

# Plot the polygon and the site point frome site info
plot(footprintdf$geom_poly[1:20])
points(x= c(easting_site), y=c(northing_site), add=TRUE)

poly <- footprintdf$geom_poly[1]

# f <- data.frame(
#   lon = as.numeric(footprintinfo[1,c(5, 7)]),
#   lat = as.numeric(footprintinfo[1,c(4, 6)])
# )

#spTransform(xy, CRS(paste("+proj=utm +zone=",utm_zone_site," ellps=WGS84",sep=''))
#Converting fluxtower coordinates to UTM
#utmcor<-LongLatToUTM(points$lon, points$lat, zone=17)

# #Read in Neon AOP data
# dpID <- "DP1.30003.001"
# siteID <- "MLBS"
# eastings<- c(min(utmcor$X), min(utmcor$X),max(utmcor$X),max(utmcor$X)) #to get the entire extent of the footprint
# northings<-c(min(utmcor$Y), max(utmcor$Y),min(utmcor$Y), max(utmcor$Y)) #to get the entire extent of the footprint
# byTileAOP(dpID=dpID,
#           site=siteID,
#           year="2021",
#           easting=eastings,
#           northing=northings,
#           check.size = FALSE) 

#Read in Neon AOP data
dpID <- "DP1.30003.001"

# siteID <- "MLBS"
# eastings<- c(min(utmcor$X), min(utmcor$X),max(utmcor$X),max(utmcor$X)) #to get the entire extent of the footprint
# northings<-c(min(utmcor$Y), max(utmcor$Y),min(utmcor$Y), max(utmcor$Y)) #to get the entire extent of the footprint

byTileAOP(dpID=dpID,
          site=site_oi,
          year=yr,
          easting=easting_site,
          northing=northing_site,
          buffer = buffersize,
          savepath=datadir,
          check.size = FALSE)


#Lidar data as a LASCatalog (all n files are loaded)
lidar_filepath <- paste("DP1.30003.001/neon-aop-products", 
                        '/', yr,  
                        '/', 'FullSite',
                        '/', domain_id_site, 
                        '/', paste(yr, '_', site_oi,'_', '4', sep=''), 
                        '/L1/DiscreteLidar/ClassifiedPointCloud/', sep='')
#ctg<-readLAScatalog("DP1.30003.001/neon-aop-products/2021/FullSite/D07/2021_MLBS_4/L1/DiscreteLidar/ClassifiedPointCloud/")

ctg<-readLAScatalog(file.path(datadir, lidar_filepath))


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





