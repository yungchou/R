# For finding global.R
setwd('N:/code/R/shiny/shiny.iot.base.v2/')
# Global.R points the working directory to './data'
source('global.R')
longitude=c(-110,-90,-100,-85)
latitude=c(33,40,37,32)
deviceID=c("rp1","rp2","rp3","rp4")
device <- cbind (deviceID, longitude, latitude)

i=0
while(i<=3600){
  df=data.frame(
      timestamp=as.numeric(Sys.time()),
      deviceID=deviceID,
      longitude=longitude,
      latitude=latitude,
      temperature=rnorm(n=4,mean=30,sd=1))
    write.csv(df,
              paste0('iot_',gsub("[^0-9]","",Sys.time()),".csv"),
              row.names = FALSE)
    i=i+1
  Sys.sleep(2)
}

while(TRUE){
  df=data.frame(
    timestamp=as.numeric(Sys.time()),
    device=deviceID[1:3],
    longitude=longitude[1:3],
    latitude=latitude[1:3],
    temperature=rnorm(n=3,mean=30,sd=1))
  write.csv(df,
            paste0(device,'_',gsub("[^0-9]","",Sys.time()),".csv"),
            row.names = FALSE)
  Sys.sleep(2)
}

