# calculate five year mean SST values for Persian Gulf coral study ---------------------- 
rm(list=ls()) #remove previous variable assignments

# load libraries
library(lubridate)
library(RCurl)
library(raster)
library(ncdf4)
library(dplyr)
library(tidyr)

# load site data
pgsites <- read.csv("UAE-Oman survey sites_7.31.19.csv", head=T, stringsAsFactors = F)

# format date
pgsites$Date <- as.Date(pgsites$Date, "%m.%d.%y")

# find date ranges
pgsites$earliestDate <- pgsites$Date - years(5)
pgsites$latestDate <- pgsites$Date - days(1)

earliestDate <- min(pgsites$earliestDate)
latestDate <- max(pgsites$latestDate)

# create vector of dates
dateRange <- seq(earliestDate, latestDate, by="days")
dateRange <- gsub("-", "", dateRange) # remove hyphens from dates

# create folder for file downloads if it does not already exist
subDir <- "coralTemp_ncFiles"

if (!file.exists(subDir)){
  dir.create(file.path(subDir))
}

# download coralTemp data from ftp server (takes several hours)
for (i in 1:length(dateRange)){ 
  YR <- substr(dateRange[i], 1, 4)
  coralTemp.URL <- paste0("ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/coraltemp/v1.0/nc/", YR, "/coraltemp_v1.0_", dateRange[i], ".nc")
  fileName <- paste0(subDir, "/coraltemp_", dateRange[i], ".nc")
  download.file(coralTemp.URL, mode="wb", fileName)
}

# create new file to store daily temperautre data 
newFileName <- "sst_persian_gulf.csv"
sst.df <- data.frame(matrix(ncol=3, nrow=0))
colnames(sst.df) <- c("Site", "Date", "SST")
write.csv(sst.df, newFileName, row.names = F)

# extract daily temperature data for all sites from coralTemp and save in "sst_persian_gulf.csv"
ncFiles <- list.files(subDir)

for (j in 1:length(ncFiles)){ # this loop takes ~10 minutes to complete
  ncFileName <- paste0(subDir, "/", ncFiles[j])
  ncTempBrick <- brick(ncFileName)
  surveySST <- extract(ncTempBrick, cbind(pgsites$Longitude..E., pgsites$Latitude..N.))
  temp.df <- data.frame("Site"=pgsites$Site, "Date"=substr(ncFiles[j],11,18), "SST"=surveySST[,])
  write.table(temp.df, file=newFileName, row.names = F, sep = ",", col.names = !file.exists(newFileName), append = T)
  # cat("fininshed", ncFiles[j], "\n) # uncomment if you want a print message of progress
  # do not worry about warning messages, they are providing information about the "brick" function
}

# format and merge sst.df data with original survey data
sst.df <- read.csv(newFileName, head=T, stringsAsFactors=F)
sst.df$Date <- paste(substr(sst.df$Date,1,4), substr(sst.df$Date,5,6), substr(sst.df$Date,7,8), sep="-")
sst.df$Date <- as.Date(sst.df$Date, "%Y-%m-%d")
sst.df$Year <- format(sst.df$Date, "%Y") 
sst.df <- merge(sst.df, pgsites[,c("Site", "earliestDate", "latestDate")], by="Site")
sst.df <- sst.df %>% group_by(Site) %>% filter(Date >= earliestDate & Date <= latestDate)

# save intermediate SST data
write.csv(sst.df, "sst_persian_gulf_five_year_daily_data.csv", row.names=F)

# calculate coralTemp metrics by site and year
sst.by.yr.site <- sst.df %>%
  group_by(Site, Year) %>%
  summarise(annual.min = min(SST), annual.max = max(SST), annual.range = max(SST)-min(SST), annual.var = var(SST))

# calculate average annual coralTemp metrics by site
sst.by.site <- sst.by.yr.site %>%
  as_tibble() %>%
  group_by(Site) %>%
  summarise(mean_annual_minimum_SST = mean(annual.min), mean_annual_maximum_SST = mean(annual.max), mean_annual_SST_range = mean(annual.range), mean_annual_SST_variance = mean(annual.var)) %>%
  mutate_at(vars(mean_annual_minimum_SST, mean_annual_maximum_SST, mean_annual_SST_range, mean_annual_SST_variance), funs(round(., 2)))

# save data
write.csv(sst.by.site, "SST_metrics_Persian_Gulf.csv", row.names=F)
