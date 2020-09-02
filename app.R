rm(list = ls())
library(devtools)
install_github('lchiffon/REmap')
devtools::install_github('madlogos/recharts')
install.packages("echarts4r")
install.packages("Hmisc")
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
library(Hmisc)
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

# creat cv base map 
basemap = leaflet(covid4251) %>% 
  addTiles() %>% setView(lng = mean(as.numeric(covid4251$Longitude)), lat = mean(as.numeric(covid4251$Latitude)), zoom = 2) %>%
  addLayersControl(
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE))   %>%
  addProviderTiles(providers$CartoDB.Positron) 



shinyApp(
# create Shiny ui
ui = shinyUI( 
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, id="nav","flight line tracker",
                tabPanel("flight line plot",
                 radioButtons("type","please select the date:",c("Jan25","April25"),inline=TRUE),
                 radioButtons("airport1","please select the orgin or destination:",c("ORIGIN","DEST"),inline=TRUE)),
                 tabPanel("flight line mapper",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 20, width = 250, fixed=TRUE,
                                            draggable = TRUE, height = "auto",uiOutput("e"),uiOutput("f"),
                                            plotlyOutput("plotget")
                          )
                 )),
                tabPanel("flight line table",
                                        
                                        sidebarLayout(
                                          sidebarPanel(
                                            uiOutput("a")),
                                          mainPanel(
                                            DTOutput("table")
                                              
                                            )
                                          )
                                        )
                 )),


server = function(input, output) {

  
  
  Pass <- reactive({
    if(input$type=="Jan25"){
      jan25_flight
    } else if (input$type=="April25"){
      April25_flight
    } 
  })
  
  Pass1 <- reactive({
    if(input$type=="Jan25"){
      jan25_flight1
    } else if (input$type=="April25"){
      April25_flight1
    } 
  })
  
  output$a <- renderUI({
    if (input$airport1=="ORIGIN") {
      uiOutput("b")
    } else if (input$airport1=="DEST"){
      uiOutput("c")
    }
  })
  
  
  output$b <- renderUI({
    pickerInput("ORIGIN1", "please select ORIGIN airport:",   
                choices = unique(Pass()$ORIGIN), 
                selected = unique(Pass()$ORIGIN)[1],
                multiple = FALSE)
  })
  output$c <- renderUI({
    pickerInput("DEST1", "please select DESTINATION airport:",   
                choices = unique(Pass()$DEST), 
                selected = unique(Pass()$DEST)[1],
                multiple = FALSE)
  })

  output$table <- renderDT({
    if (input$airport1=="ORIGIN") {
    Pass()%>%filter(ORIGIN%in%input$ORIGIN1)%>%arrange(desc(n))
    } else if (input$airport1=="DEST"){
    Pass()%>%filter(DEST%in%input$DEST1)%>%arrange(desc(n))
    }
  })
  
  output$e <- renderUI({
    if (input$airport1=="ORIGIN") {
      uiOutput("b1")
    } else if (input$airport1=="DEST"){
      uiOutput("c1")
    }
  })
  
  output$b1 <- renderUI({
    pickerInput("ORIGIN11", "please select ORIGIN city:",   
                choices = unique(Pass1()$ORIGIN_STATE_NM), 
                selected = unique(Pass1()$ORIGIN_STATE_NM)[1],
                multiple = FALSE)
  })
  output$c1 <- renderUI({
    pickerInput("DEST11", "please select DESTINATION city:",   
                choices = unique(Pass1()$DEST_STATE_NM), 
                selected = unique(Pass1()$DEST_STATE_NM)[1],
                multiple = FALSE)
  })
  
  output$plotget <- renderPlotly({
    if (input$airport1=="ORIGIN"){
      dataget <- Pass1()%>%filter(ORIGIN_STATE_NM%in%input$ORIGIN11)
      class(dataget) <- "data.frame"
      plot_ly(data=dataget,values=~n,labels=~DEST_STATE_NM,type="pie",showlegend=FALSE)
    } else if (input$airport1=="DEST"){
      dataget <-Pass1()%>%filter(DEST_STATE_NM%in%input$DEST11)
      class(dataget) <- "data.frame"
      plot_ly(data=dataget,values=~n,labels=~ORIGIN_STATE_NM,type="pie",showlegend=FALSE)
    }
    })
  
  output$f <- renderUI({
    if (input$airport1=="ORIGIN") {
      uiOutput("b2")
    } else if (input$airport1=="DEST"){
      uiOutput("c2")
    }
  })
  
  output$b2 <- renderUI({
    pickerInput("ORIGIN12", "please select ORIGIN airport:",   
                choices = unique(Pass1()[Pass1()$ORIGIN_STATE_NM==input$ORIGIN11,]$ORIGIN), 
                selected = unique(Pass1()[Pass1()$ORIGIN_STATE_NM==input$ORIGIN11,]$ORIGIN)[1],
                multiple = FALSE)
  })
  output$c2 <- renderUI({
    pickerInput("DEST12", "please select DESTINATION airport:",   
                choices = unique(Pass1()[Pass1()$DEST_STATE_NM==input$DEST11,]$DEST), 
                selected = unique(Pass1()[Pass1()$DEST_STATE_NM==input$DEST11,]$DEST)[1],
                multiple = FALSE)
  })
  
  
  
  
Passget <- reactive({
  if (input$airport1=="ORIGIN") {
    Pass1()%>%filter(ORIGIN%in%input$ORIGIN12)
  } else if (input$airport1=="DEST"){
    Pass1()%>%filter(DEST%in%input$DEST12)
  }

})
  

output$mymap <- renderLeaflet({ 


    Pass1 <- Passget()
    class(Pass1) <- "data.frame"
    flows <- gcIntermediate(select(Pass1,Longitude.x, Latitude.x) , select(Pass1,Longitude.y, Latitude.y), n=200 , sp = TRUE, addStartEnd = TRUE) 
    #load icon for dest marker 
    icons <- makeAwesomeIcon(
      icon = 'ion-plane',
      iconColor = 'blue',
      library = 'ion',
      markerColor = "blue")
    
    #load icon for origin marker 
    OriginIcon <- makeAwesomeIcon(
      icon = 'ion-plane',
      iconColor = 'red',
      library = 'ion',
      markerColor = "red")
    
    #cut the suicide rate into 5 levels
    covid4251$Confirmed_cut <- cut2(covid4251$Confirmed,g=5)
    #add colour brewer
    pal <- colorFactor(as.character(lines_color$color), domain = covid4251$Confirmed_cut)
    
    
    basemap %>% 
      clearShapes() %>%
      addLegend(position = "bottomleft",pal=pal,values = covid4251$Confirmed_cut)%>%
      addCircles(data = covid4251, lat = covid4251$Latitude, lng =covid4251$Longitude, weight = 1, radius = 100000,color = pal(covid4251$Confirmed_cut) ,fillOpacity = 1,stroke = FALSE, group = "2020-COVID (confirm)",
                 label = sprintf("<strong>%s </strong><br/>cum cofirm cases:  %d", covid4251$`Province/State`,covid4251$Confirmed) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = "blue"),
                   textsize = "15px", direction = "auto")) %>%
      addTiles()%>%addAwesomeMarkers(lng = Pass1$Longitude.x, lat = Pass1$Latitude.x, icon=OriginIcon, layerId = Pass1$ORIGIN_STATE_NM, label = paste(Pass1$ORIGIN_STATE_NM)) %>%addAwesomeMarkers(lng = Pass1$Longitude.y, lat = Pass1$Latitude.y, icon=icons, layerId = Pass1$DEST_STATE_NM, label =paste0(Pass1$DEST_STATE_NM,":",Pass1$n))%>%  addPolylines(data = flows,color = "grey",weight = 4,dashArray = "15, 10, 5, 10") 
    
  })
  
}

,
options=list(
  width="120%", height=2400)
)
