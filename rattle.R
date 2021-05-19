#----------
# PACKAGES
#----------
pkgs <- c( 
   "shiny","ggplot2","plotly","dygraphs"
  ,"tidyr","dplyr"
  ,"rattle","magrittr"
  ,"readr","readxl","xlsx"
#  ,"usmap"
#  ,"proto"
#  ,"gsubfn"
#  ,"stringr"
#  ,"RCurl"
#  ,"RJSONIO"
#  ,"sqldf"
  )
for (i in pkgs){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
  }
  require(i)
}


s <- "string manipulation"
substr(s, start=3, stop=6)
