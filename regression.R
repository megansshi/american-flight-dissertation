
library(readr)
library(leaflet)
library(dplyr)
library(Hmisc)
library(corrplot)
setwd("/Users/megansshi/Desktop/Air")
data <- read_csv("data.csv")
covid4251<- read_csv("covid4251.csv")
covid4251 <- covid4251%>%filter(!is.na(`Province/State`))%>%group_by(`Province/State`)%>%summarise(Latitude=mean(Latitude),Longitude=mean(Longitude),Confirmed=sum(Confirmed))
covid4251$Confirmed_cut <- cut2(covid4251$Confirmed,g=5)
lines_color <- data.frame(color=RColorBrewer::brewer.pal(n = 5, name = 'RdBu')) 
pal <- colorFactor(as.character(lines_color$color), domain = covid4251$Confirmed_cut)

write.csv(covid4251,file="covid4251.csv")
#covid19 map


# creat cv base map 
leaflet(covid4251) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE))   %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  clearShapes() %>%
  addLegend(position = "bottomleft",pal=pal,values = covid4251$Confirmed_cut)%>%
  addCircles(data = covid4251, lat = covid0425$Latitude, lng =covid4251$Longitude, weight = 1, radius = 300000,color = pal(covid4251$Confirmed_cut) ,fillOpacity = 1,stroke = FALSE, group = "2020-COVID (confirm)",
             label = sprintf("<strong>%s </strong><br/>cum cofirm cases:  %d", covid0425$`Province/State`,covid4251$Confirmed) %>% lapply(htmltools::HTML),
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = "blue"),
               textsize = "15px", direction = "auto"))
#clustering analysis


set.seed(1)
data$county <- gsub(" County","",data$county)
colnames(covid4251)[1] <- "county"
data <- inner_join(data,covid4251)
kmean <- kmeans(data%>%select(grep("^trips",colnames(data))),3)
kmean
#divided routes into three categories, the first one is the not too busy, second is busiest(most of them are long-range routes.), third one is the busier one. 
data$cluster <- kmean$cluster

#regression analysis


model <- lm(Confirmed~pop_stay_at_home+pop_not_stay_at_home+factor(cluster)+0,data=data)
summary(model)



#Correlation of variables (scatter plot)

print(pairs(~Confirmed+pop_stay_at_home+pop_not_stay_at_home+cluster,data=data))


#Correlation of variables (correlation coefficient) :

print(corrplot(data%>%select(Confirmed,pop_stay_at_home,pop_not_stay_at_home,cluster)%>%cor()))


#R-square =79.87%
