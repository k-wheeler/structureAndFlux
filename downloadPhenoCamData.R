library('tidyverse')
library('phenocamr')

availableFluxYears <- read.csv('availableFluxYears_condensed.csv')
allSiteData <- read.csv('NEON_Field_Site_Metadata_20230309.csv') %>% 
  filter(field_site_type %in% c("Gradient Terrestrial","Core Terrestrial"))
phenoCamDataFolder <- 'phenocamData'

#Download PhenoCam Data ----
for(s in 1:nrow(allSiteData)){
  siteID=allSiteData$field_site_id[s]
  print(siteID)
  pcTop_siteID <- paste0('NEON.',allSiteData$field_domain_id[allSiteData$field_site_id==siteID],".",
                         siteID,'.DP1.00033$')
  pcBottom_siteID <- paste0('NEON.',allSiteData$field_domain_id[allSiteData$field_site_id==siteID],".",
                            siteID,'.DP1.00042$')
  files <- dir(path=phenoCamDataFolder,pattern=siteID)
  if(length(files)==0){
    download_phenocam(
      site = pcTop_siteID,
      veg_type = NULL,
      frequency = "3",
      roi_id = NULL,
      outlier_detection = TRUE,
      smooth = TRUE,
      contract = FALSE,
      daymet = FALSE,
      trim_daymet = TRUE,
      trim = NULL,
      phenophase = FALSE,
      out_dir = phenoCamDataFolder,
      internal = FALSE
    )
    download_phenocam(
      site = pcBottom_siteID,
      veg_type = NULL,
      frequency = "3",
      roi_id = NULL,
      outlier_detection = TRUE,
      smooth = TRUE,
      contract = FALSE,
      daymet = FALSE,
      trim_daymet = TRUE,
      trim = NULL,
      phenophase = FALSE,
      out_dir = phenoCamDataFolder,
      internal = FALSE
    )
  }
}

