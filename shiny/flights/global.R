library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)

flights <- fread(file = "./flights14.csv")
#flights <- read.csv(file = "./flights14.csv")