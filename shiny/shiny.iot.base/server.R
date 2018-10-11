
threshold = 33


filenames <- list.files(pattern="*.csv", full.names=TRUE)
data_at_start <- rbindlist(lapply(filenames, fread))
ids_at_start <- unique(data_at_start$sensorID)

IsThereNewFile <- function(){

  filenames <- list.files(pattern="*.csv", full.names=TRUE)
  length(filenames)
}

ReadAllData=function(){ # A function that calculates the underlying value

  filenames <- list.files(pattern="*.csv", full.names=TRUE)
  temp= rbindlist(lapply(filenames, fread))
  temp$timestamp =as.POSIXct(as.numeric(as.character(temp$timestamp)),origin="1970-01-01",tz="GMT")
  temp
}

shinyServer(function(input, output,session) {

  alldata <- reactivePoll(10, session, IsThereNewFile, ReadAllData)
  # 10: polling IsThereNewFile every 10 milliseconds

  # Map

    output$mytest <- renderLeaflet({

    leaflet() %>%
    addTiles() %>%
    setView(-96, 37.8, 4) %>%
    addMarkers(
      lng=-99.3268, lat=38.8792,
      popup="<b>Howdy from</b><br><a href='http://www.haysusa.com/'>Hays, KS</a>")
  })

  output$myleaflet <- renderLeaflet({

    latslons=distinct(data_at_start, longitude, latitude, .keep_all=TRUE)
    latslons$latlon=paste(latslons$longitude, latslons$latitude, sep=' , ')

    top = max(latslons$latitude)+2
    left = max(latslons$longitude)-2
    right = max(latslons$latitude)+2
    bottom = max(latslons$longitude)-2

    leaflet(latslons) %>%
      fitBounds(right,bottom,left,top) %>%
      addTiles() %>%
      addMarkers(
        data=latslons,
        label=~as.character(sensorID),
        labelOptions = labelOptions(noHide = T,textOnly = T,textsize = "16px",offset = c(12, -10)) ) %>%
      addMarkers(
        data=latslons,
        label=~as.character(latlon),
        labelOptions = labelOptions(noHide = F,textOnly = T,textsize = "12px",offset = c(12, 10))
      )
  })

  # Timeseries
  output$timeseries_all = renderPlot({
    dat=alldata()
    end=nrow(dat)
    start=1  #end-100

    if(nrow(dat)>=1){
      dat[start:end,]%>%ggplot(aes(x=timestamp,y=temperature))+
        geom_line(aes(color=sensorID))+ylim(26, 34)+
        geom_hline(yintercept = threshold,linetype="dotted",color="darkblue")+
        labs(x="",y="Temperature",color="Sensor IDs")+
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(colour="blue",size=14),
              axis.text = element_text(colour="darkred",size=12),
              plot.title = element_blank())
    }
  })

  ids_too_high_reading=reactive({
    dat=alldata()
    temp=filter(dat,timestamp==max(dat$timestamp))
    ids=temp$sensorID[temp$temperature>100]
    ids

  })

  ids_failed_sensors=reactive({
    dat=alldata()
    temp=filter(dat,timestamp==max(dat$timestamp))
    ids=unique(temp$sensorID)
    previous_ids=new_ids$ids
    previous_ids[!(previous_ids %in% ids)]

  })

  observe({
    if(length(ids_too_high_reading())>0){

      # Verizon: number@vtext.com
      # AT&T: number@txt.att.net
      # other carriers: https://20somethingfinance.com/how-to-send-text-messages-sms-via-email-for-free/

    #   send.mail(from = sender,
    #             to = recipients,
    #             subject="Sensor Alert: Abnormal reading",
    #             body =paste("Abnormal reading! Sensor IDs: ",ids_too_high_reading()),
    #             smtp = list(host.name = "smtp.gmail.com", port = 465,
    #                         user.name="sender", passwd="password", ssl=TRUE),
    #             authenticate = TRUE,
    #             send = TRUE)

        }

  })

  new_ids=reactiveValues(ids=ids_at_start)

  observe({
    if(length(ids_failed_sensors())>0){
      new_ids$ids=new_ids$ids[!(new_ids$ids %in% ids_failed_sensors())]

      # send.mail(from = sender,
      #           to = recipients,
      #           subject="Sensor Alert:Failed Sensors",
      #           body =paste("Failed Sensors! Sensor IDs: ",ids_failed_sensors()),
      #           smtp = list(host.name = "smtp.gmail.com", port = 465,
      #                       user.name="sender email", passwd="password", ssl=TRUE),
      #           authenticate = TRUE,
      #           send = TRUE)

    }

  })

  output$Too_High <-  renderText({

    paste("Sensors with too high Readings: ",length(ids_too_high_reading()))
  })

  output$Failed_Sensors <-  renderText({

    paste("Number of Failed Sensors: ",length(ids_failed_sensors()))
  })

  # reactive that generates a random value for the gauge
  value = reactive({
    invalidateLater(1000)
    round(runif(1,0,100),2)
  })

  # example use of the automatically generated render function
  output$gauge1 <- renderC3Gauge({
    # C3Gauge widget
    C3Gauge(value())
  })

})