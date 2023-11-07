#Load Libraries ----
library('tidyverse')
library('phenocamr')

phenoCamDataFolder <- 'phenocamData'
phenoCamTransitionFolder <- paste0(phenoCamDataFolder,"/","transitionDates")

availableFluxYears <- read.csv('availableFluxYears_condensed.csv')
allSiteData <- read.csv('NEON_Field_Site_Metadata_20230309.csv') %>% 
  filter(field_site_type %in% c("Gradient Terrestrial","Core Terrestrial"))
desiredSites <- unique(availableFluxYears$field_site_id)

#Calculate transition dates ----
#Note: This is done outside of the download_phenocam function so that we can specify different max gcc_90 threshold (e.g.,90% change)
files <- dir(path=phenoCamDataFolder,pattern="DP1.00")
for(f in seq_along(files)){
  dat <- read_phenocam(file.path(phenoCamDataFolder,files[f]))
  phenophases(data = dat,
              internal = FALSE,
              out_dir = phenoCamTransitionFolder,
              lower_thresh = 0.15,
              middle_thresh = 0.5,
              upper_thresh = 0.85
  )
}

#Summary Stats for each site-year ----
summaryTranData <- matrix(nrow=0,ncol=10)
for(s in seq_along(desiredSites)){
  siteID <- desiredSites[s]
  print(siteID)
  files <- dir(path=phenoCamTransitionFolder,pattern=siteID)
  if(length(files)>0){
    dat <- read.csv(paste0(phenoCamTransitionFolder,"/",files[1]),skip=16)
    if(length(files)>1){
      for(f in 2:length(files)){
        dat <- rbind(dat,read.csv(paste0(phenoCamTransitionFolder,"/",files[f]),skip=16))
      }
    }
    desiredYears <- availableFluxYears$year[availableFluxYears$field_site_id==siteID]
    for(yr in desiredYears){
      yearDat <- dat %>% mutate(year=lubridate::year(transition_50)) %>%
        filter(year == yr,gcc_value=="gcc_90")
      risingDat <- yearDat %>% filter(direction=="rising") %>%
        summarise(n=n(),risingStart=min(transition_15),risingEnd=max(transition_85),risingSd=round(sd(as.Date(transition_50)),digits=4))
      fallingDat <- yearDat %>% filter(direction=="falling") %>%
        summarise(n=n(),fallingStart=min(transition_85),fallingEnd=max(transition_15),fallingSd=round(sd(as.Date(transition_50)),digits=4))
      summaryTranData <- rbind(summaryTranData,c(siteID,yr,as.character(risingDat),as.character(fallingDat)))
    }
  }else{
    summaryTranData <- rbind(summaryTranData,c(siteID,yr,0,NA,NA,NA,0,NA,NA,NA))
  }
  
}
colnames(summaryTranData) <- c('siteID','year','risingCount','risingStart','risingEnd','risingSd','fallingCount','fallingStart','fallingEnd','fallingSd')

#Save phenocam transition dates ----
write.csv(file="summaryPhenocamTransitions.csv",summaryTranData,row.names = FALSE,quote=FALSE)
