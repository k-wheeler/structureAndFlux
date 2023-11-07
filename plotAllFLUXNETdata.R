library('tidyverse')

#Read in Site Data and Set Folder Name ----
amerifluxNameDat <- read.csv('neon_ameriflux_names.csv')
amerifluxDataFolder <- "NEON_EddyCov_FLUXNET_Data/"
phenoDates <- read.csv('summaryPhenocamTransitions.csv')

#Function to extract Ameriflux date from given timestamp
extractAmeriDte <- function(X){
  yr <- substr(X,1,4)
  mth <- substr(X,5,6)
  dy <- substr(X,7,8)
  return(paste(yr,mth,dy,sep="-"))
}

#Loop over NEON sites that have been processed for fluxnet and plot data ----
pdf('NEON_fluxnetData.pdf',width=10,height=3)
for(s in 1:nrow(amerifluxNameDat)){
  print(s)
  ameriDat <- read.csv(paste0(amerifluxDataFolder,dir(path=amerifluxDataFolder,pattern=amerifluxNameDat$ameriflux_site_id[s])))
  ameriDat$GPP_NT_VUT_50[ameriDat$GPP_NT_VUT_50==-9999] <- NA
  ameriDat$date <- as.Date(as.character(lapply(ameriDat$TIMESTAMP,extractAmeriDte)))
  
  sitePheno <- phenoDates %>% filter(siteID==amerifluxNameDat$neon_site_id[s])
  plot(ameriDat$date,ameriDat$GPP_NT_VUT_50,pch=20,xlab="Date",ylab="GPP",main=amerifluxNameDat$neon_site_id[s])
  abline(v=as.Date(sitePheno$risingStart),col="green")
  abline(v=as.Date(sitePheno$risingEnd),col="green")
  abline(v=as.Date(sitePheno$fallingStart),col="orange")
  abline(v=as.Date(sitePheno$fallingEnd),col="orange")
}
dev.off()
