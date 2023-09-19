##Script for downloading flux data for determining flux footprint
library('neonUtilities')
library('neonOS')
library('tidyverse')
options(timeout=600)

#Site Information ----
allSiteData <- read.csv('NEON_Field_Site_Metadata_20230309.csv') %>% 
  filter(field_site_type %in% c("Gradient Terrestrial","Core Terrestrial")) #Downloaded from https://www.neonscience.org/field-sites/explore-field-sites

availableFluxYears <- read.csv('availableFluxYears.csv')
fluxSaveFolder <- "NEON_EddyCov_Data/"

#Download and Stack NEON Data ----
if(file.exists('/Users/Kathryn/Documents/NEON_Token.R')){
  source('/Users/Kathryn/Documents/NEON_Token.R')
  for(i in 1:nrow(availableFluxYears)){
    siteName=availableFluxYears$field_site_id[i]
    year=availableFluxYears$year[i]
    if(length(dir(path=paste0(fluxSaveFolder,siteName,"/filesToStack00200"),pattern=paste0(siteName,".DP4.00200.001.",year)))<10){
      print(siteName)
      print(year)
      zipsByProduct(dpID="DP4.00200.001",
                    site=siteName,
                    check.size = F,
                    package="basic",
                    savepath=paste0(fluxSaveFolder,siteName),
                    startdate=paste0(year,"-01-01"),
                    enddate=paste0(year,"-12-31"),
                    token = NEON_TOKEN)
    }
  }
}else{
  for(i in 1:nrow(availableFluxYears)){
    siteName=availableFluxYears$field_site_id[i]
    year=availableFluxYears$year[i]
    if(length(dir(path="filesToStack00200",pattern=paste0(siteName,".DP4.00200.001.",year)))<10){
      print(siteName)
      print(year)
      zipsByProduct(dpID="DP4.00200.001",
                    site=siteName,
                    check.size = F,
                    package="basic",
                    startdate=paste0(year,"-01-01"),
                    enddate=paste0(year,"-12-31"))
    }
  }
}
