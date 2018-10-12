library(dplyr)
library(ggplot2)
library(dygraphs)
library(datasets)

#------iot
library(anytime)
library(tidyjson)

df <- na.omit( read.csv('myIoTSensorData.csv', header = TRUE) ) %>%
  select( Timestamp, message ) %>%
  mutate( Timestamp = anytime( as.factor(Timestamp) ) ) %>%
  mutate( message = as.character(message) )

df1 <- df$message %>% as.tbl_json %>%
  gather_array %>%
  spread_values(
    msgID = jstring("messageId"),
    devID = jstring("deviceId"),
    temperature = jnumber("temperature"),
    humidity = jnumber("humidity")) %>%
  select(devID, temperature, humidity)

iot_ID <- df1$devID
iot_temp <- df1$temperature[1:3062]
iot_humi <- df1$humidity

df$Timestamp

plot(df$Timestamp, iot_temp) +
  geom_line() +
  theme_classic()
  scale_x_datetime(date_labels = "%p-%d", date_breaks = "36 hour") +
