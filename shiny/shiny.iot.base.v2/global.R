# Data repository
#setwd("./data")

library(dplyr)
library(ggplot2)
library(data.table)
library(leaflet)
library(mailR)

#devtools::install_github("FrissAnalytics/shinyJsTutorials/widgets/C3")
library(C3)

#-------------------
# For IoT Simulator
#-------------------
threshold = 33

# Test IoT Devices
longitude=c(-121,-91,-100,-85)
latitude=c(23,40,37,22)
deviceID=c("rp1","rp2","rp3","rp4")
device <- cbind(deviceID, longitude, latitude)

# SAMPLE DATA
filenames <- list.files(pattern="*.csv", full.names=TRUE)
data_at_start <- rbindlist(lapply(filenames, fread))
ids_at_start <- unique(data_at_start$deviceID)

IsThereNewFile <- function(){

  filenames <- list.files(pattern="*.csv", full.names=TRUE)
  length(filenames)
}

ReadAllData=function(){

  filenames <- list.files(pattern="*.csv", full.names=TRUE)
  temp= rbindlist(lapply(filenames, fread))
  temp$timestamp =as.POSIXct(as.numeric(as.character(temp$timestamp)),origin="1970-01-01",tz="GMT")
  temp
}

