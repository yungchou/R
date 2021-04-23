`# Library
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

library(dygraphs)
library(xts)    # To make the conversion data-frame / xts format

sim_rt_files <- read.csv(
  'N:/code/R/shiny/shiny.iot.base.v2/iot_sample_data.csv',
  header = TRUE)

source <- sim_rt_files

df <- na.omit(source) %>%
  select( Timestamp, message ) %>%
  mutate( Timestamp = anytime( as.character(Timestamp) ) ) %>%
  arrange( Timestamp ) %>%
  mutate( message = as.character(message) )

df1 <- df$message %>% as.tbl_json %>%
  gather_array %>%
  spread_values(
    msgID = jstring("messageId"),
    devID = jstring("deviceId"),
    temperature = jnumber("temperature"),
    humidity = jnumber("humidity")) %>%
  select(devID, temperature, humidity)

n=100

iot_time <- df$Timestamp[1:n]
iot_ID <- df1$devID[1:n]
iot_temp <- df1$temperature[1:n]
iot_humi <- df1$humidity[1:n]

ambient <- cbind(iot_temp,iot_humi)
ambient <- xts(ambient, order.by = iot_time)

dygraph(ambient,
        main='Simulated Realtime Reading for IoT Stream Data',
        group=ts) %>%
  dySeries("iot_temp", label='Temperature') %>%
  dySeries("iot_humi", axis='y2', label='Humidity') %>%
  dyAxis("y",  label="Temperature (C)", independentTicks=TRUE) %>%
  dyAxis("y2", label="Humidity (%)") %>%
  dyOptions( fillGraph=TRUE, fillAlpha=0.5) %>%
  dyHighlight(
    highlightCircleSize=5,
    highlightSeriesBackgroundAlpha=0.2,
    hideOnMouseOut=FALSE) %>%
  dyRoller(rollPeriod=5) %>%
  dyOptions(colors=RColorBrewer::brewer.pal(3, "Dark2")) %>%
  dyRangeSelector(height=20) %>% dyUnzoom() %>%
  dyCrosshair(direction="vertical") %>%
  dyLegend(show = "follow")


