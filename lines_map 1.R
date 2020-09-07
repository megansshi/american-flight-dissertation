rm(list = ls())
library(readr)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)
library(reshape)
library(shinyWidgets)
library(DT)
library(geosphere)
library(htmlwidgets)
library(echarts4r)
setwd("/Users/megansshi/Desktop/Air")
airport <- read_csv("usaairports.csv")
class(airport) <- "data.frame"
routes <- read_csv("routes.csv")
April25 <- read_csv("425aftercancel.csv")
jan25 <- read_csv("jan25.csv")


jan25$FL_DATE <- dmy(jan25$FL_DATE)
jan25 <- jan25%>%filter(FL_DATE=="2020-01-25")
jan25_flight <- jan25%>%group_by(ORIGIN,ORIGIN_STATE_NM,DEST,DEST_STATE_NM)%>%count()



April25$FLDATE <- dmy(April25$FLDATE)
April25_flight <- April25%>%group_by(ORIGIN,ORIGINSTATENM,DEST,DESTSTATENM)%>%count()


jan25_flight1 <- inner_join(jan25_flight,airport,by=c("ORIGIN"="IATA_CODE"))
jan25_flight1 <- inner_join(jan25_flight1,airport,by=c("DEST"="IATA_CODE"))
jan25_flight1$DISTANCE <- sapply(1:nrow(jan25_flight1),function(x) geosphere::distm(c(jan25_flight1$LONGITUDE.x[x],jan25_flight1$LATITUDE.x[x]),c(jan25_flight1$LONGITUDE.y[x],jan25_flight1$LATITUDE.y[x])))



class(jan25_flight1) <- "data.frame"
April25_flight1 <- left_join(April25_flight,airport,by=c("ORIGIN"="IATA_CODE"))
April25_flight1 <- left_join(April25_flight1,airport,by=c("DEST"="IATA_CODE"))%>%dplyr::rename(ORIGIN_STATE_NM=ORIGINSTATENM,DEST_STATE_NM=DESTSTATENM)

April25_flight1$DISTANCE <- sapply(1:nrow(April25_flight1),function(x) geosphere::distm(c(April25_flight1$LONGITUDE.x[x],April25_flight1$LATITUDE.x[x]),c(April25_flight1$LONGITUDE.y[x],April25_flight1$LATITUDE.y[x])))



class(April25_flight1) <- "data.frame"

jan25_flight1 %>% 
  e_charts() %>% 
  e_geo() %>% 
  e_lines(
    LONGITUDE.x, 
    LATITUDE.x, 
    LONGITUDE.y, 
    LATITUDE.y,
    name = "flights",
    lineStyle = list(normal = list(curveness = 0.3))
  )

April25_flight1 %>% 
  e_charts() %>% 
  e_geo() %>% 
  e_lines(
    LONGITUDE.x, 
    LATITUDE.x, 
    LONGITUDE.y, 
    LATITUDE.y,
    name = "flights",
    lineStyle = list(normal = list(curveness = 0.3))
  )

