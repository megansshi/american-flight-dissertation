rm(list = ls())
library(readr)
library(dplyr)
library(lubridate)
library(leaflet)
library(reshape)
library(geosphere)
library(openxlsx)
library(igraph)
library(ggplot2)
setwd("/Users/megansshi/Desktop/Air")
lines_color <- data.frame(color=c("#ED3229","#36B854","#FFD823","#320176","#823094")) 
airport <- read_csv("airports.csv")
routes <- read_csv("routes.csv")
April25 <- read_csv("425aftercancel.csv")
jan25 <- read_csv("jan25.csv")
covid4251<- read_csv("covid4251.csv")
covid4251 <- covid4251%>%filter(!is.na(`Province/State`))%>%group_by(`Province/State`)%>%summarise(Latitude=mean(Latitude),Longitude=mean(Longitude),Confirmed=sum(Confirmed))
class(covid4251) <- "data.frame"

jan25$FL_DATE <- dmy(jan25$FL_DATE)
jan25 <- jan25%>%filter(FL_DATE=="2020-01-25")
jan25_flight <- jan25%>%group_by(ORIGIN,ORIGIN_STATE_NM,DEST,DEST_STATE_NM)%>%count()


April25$FLDATE <- dmy(April25$FLDATE)
April25_flight <- April25%>%group_by(ORIGIN,ORIGINSTATENM,DEST,DESTSTATENM)%>%count()


jan25_flight1 <- left_join(jan25_flight,airport,by=c("ORIGIN"="IATA"))
jan25_flight1 <- left_join(jan25_flight1,airport,by=c("DEST"="IATA"))
class(jan25_flight1) <- "data.frame"
April25_flight1 <- left_join(April25_flight,airport,by=c("ORIGIN"="IATA"))
April25_flight1 <- left_join(April25_flight1,airport,by=c("DEST"="IATA"))%>%dplyr::rename(ORIGIN_STATE_NM=ORIGINSTATENM,DEST_STATE_NM=DESTSTATENM)
class(April25_flight1) <- "data.frame"

jan25_flight1$DISTANCE <- sapply(1:nrow(jan25_flight1),function(x) distm(c(jan25_flight1$Longitude.x[x],jan25_flight1$Latitude.x[x]),c(jan25_flight1$Longitude.y[x],jan25_flight1$Latitude.y[x])))
April25_flight1$DISTANCE <- sapply(1:nrow(April25_flight1),function(x) distm(c(April25_flight1$Longitude.x[x],April25_flight1$Latitude.x[x]),c(April25_flight1$Longitude.y[x],April25_flight1$Latitude.y[x])))

jan25_flight1$DENSITY <- jan25_flight1$DISTANCE/jan25_flight1$n
April25_flight1$DENSITY <- April25_flight1$DISTANCE/April25_flight1$n


jan_state_distance_ORIGIN <- jan25_flight1%>%group_by(ORIGIN_STATE_NM)%>%summarise(mean_distance=mean(DISTANCE,na.rm = TRUE))
jan_state_distance_DEST <- jan25_flight1%>%group_by(DEST_STATE_NM)%>%summarise(mean_distance=mean(DISTANCE,na.rm = TRUE))
jan_state_distance_ORIGIN_GET <- data.frame(ORIGIN_STATE_NM="total",mean_distance=mean(jan25_flight1$DISTANCE,na.rm = TRUE))

ggplot(data=jan_state_distance_ORIGIN,aes(x=ORIGIN_STATE_NM,y=mean_distance,fill="red"))+geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_hline(yintercept =jan_state_distance_ORIGIN_GET$mean_distance)

jan_state_distance_ORIGIN <- plyr::rbind.fill(jan_state_distance_ORIGIN,jan_state_distance_ORIGIN_GET)

jan_state_distance_DEST_GET <- data.frame(DEST_STATE_NM="total",mean_distance=mean(jan25_flight1$DISTANCE,na.rm = TRUE))

ggplot(data=jan_state_distance_DEST,aes(x=DEST_STATE_NM,y=mean_distance,fill="red"))+geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_hline(yintercept =jan_state_distance_DEST_GET$mean_distance)


jan_state_distance_DEST <- plyr::rbind.fill(jan_state_distance_DEST,jan_state_distance_DEST_GET)



Apri_state_distance_ORIGIN <- April25_flight1%>%group_by(ORIGIN_STATE_NM)%>%summarise(mean_distance=mean(DISTANCE,na.rm = TRUE))
Apri_state_distance_DEST <- April25_flight1%>%group_by(DEST_STATE_NM)%>%summarise(mean_distance=mean(DISTANCE,na.rm = TRUE))


Apri_state_distance_ORIGIN_GET <- data.frame(ORIGIN_STATE_NM="total",mean_distance=mean(April25_flight1$DISTANCE,na.rm = TRUE))

ggplot(data=Apri_state_distance_ORIGIN,aes(x=ORIGIN_STATE_NM,y=mean_distance,fill="red"))+geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_hline(yintercept =Apri_state_distance_ORIGIN_GET$mean_distance)


Apri_state_distance_ORIGIN <- plyr::rbind.fill(Apri_state_distance_ORIGIN,Apri_state_distance_ORIGIN_GET)
Apri_state_distance_DEST_GET <- data.frame(DEST_STATE_NM="total",mean_distance=mean(April25_flight1$DISTANCE,na.rm = TRUE))

ggplot(data=Apri_state_distance_DEST,aes(x=DEST_STATE_NM,y=mean_distance,fill="red"))+geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_hline(yintercept =Apri_state_distance_DEST_GET$mean_distance)


Apri_state_distance_DEST <- plyr::rbind.fill(Apri_state_distance_DEST,Apri_state_distance_DEST_GET)




write.xlsx(jan_state_distance_ORIGIN,"jan_state_distance_ORIGIN_DISTANCE.xlsx")
write.xlsx(jan_state_distance_DEST,"jan_state_distance_DEST_DISTANCE.xlsx")
write.xlsx(Apri_state_distance_ORIGIN,"Apri_state_distance_ORIGIN_DISTANCE.xlsx")
write.xlsx(Apri_state_distance_DEST,"Apri_state_distance_DEST_DISTANCE.xlsx")




write.xlsx(jan25_flight1,"jan25_flight_DENSITY_DISTANCE.xlsx")
write.xlsx(April25_flight1,"April25_flight_DENSITY_DISTANCE.xlsx")
g <- graph_from_data_frame(jan25_flight1%>%filter(n>20)%>%select(ORIGIN,DEST))
plot(g)

g1<- graph_from_data_frame(April25_flight1%>%filter(n>7)%>%select(ORIGIN,DEST))
plot(g1)

