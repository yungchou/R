# Data repository
setwd("./data")

library(dplyr)
library(ggplot2)
library(data.table)
library(leaflet)
library(mailR)

#devtools::install_github("FrissAnalytics/shinyJsTutorials/widgets/C3")
library(C3)


# Test IoT Devices
longitude=c(-110,-90,-100,-85)
latitude=c(33,40,37,34)
sensorID=c("rp1","rp2","rp3","rp4")
device <- cbind (sensorID, longitude, latitude)

