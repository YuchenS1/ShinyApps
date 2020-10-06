library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)

#Importing pd_c
# raw <- jsonlite::fromJSON("https://pd_c.seattle.gov/resource/rke9-rsvs.json?$select=date_trunc_ymd(occupancydatetime)%20as%20datetime,%20paidoccupancy,%20blockfacename,%20parkingtimelimitcategory,%20parkingspacecount,%20paidparkingarea,%20location,%20date_extract_woy(occupancydatetime)%20as%20woy&$where=datetime%20between%20%272020-09-03%27%20and%20%272020-09-04%27&$limit=2000000")

# pd_c <- raw %>% mutate(date = as.Date(date), AVG_paidoccupancy = round(as.numeric(AVG_paidoccupancy),2), long = location$long, lat = location$lat, woy = as.numeric(woy)) %>% select(-location) %>% mutate(popup_info = paste(blockfacename,"<br/>", AVG_paidoccupancy, "cars on average"))
pd_c <- read.csv("~/Desktop/parking_data.csv")

#Clean pd_c
# coordinates <- raw$location %>% unnest(c(type, coordinates)) %>% select(coordinates) %>% split(1:2)


# raw %>% mutate(long = deframe(coordinates[[1]]), lat = deframe(coordinates[[2]]), occupancy = as.numeric(paidoccupancy), timelimit = as.numeric(parkingtimelimitcategory)/60, spaces = as.numeric(parkingspacecount)) -> raw

# raw %>% group_by(long, lat) %>% summarize(AVG_occupancy = mean(occupancy), timelimit = max(timelimit), spaces = max(spaces), area = first(paidparkingarea)) -> raw

pd_c <- pd_c %>% mutate(popup_info = paste("Average Occupancy: ", AVG_occupancy, "<br/>", "Available Spaces: ", spaces, "<br/>", "Time Limit: ", timelimit, "Hour(s)", "<br/>", "Coordinates: ", lat, ", ", long))

#UI
ui <- bootstrapPage(

      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      
      leafletOutput("seattleMap", width = "100%", height = "100%"),
      
      absolutePanel(
            top = 90, 
            left = 10, 
            width = 120,
            height = 15,
            sliderInput("AVG_car", 
                        "Average Occupied Cars", 
                        min(pd_c$AVG_occupancy), 
                        max(pd_c$AVG_occupancy),
                        value = range(pd_c$AVG_occupancy), 
                        step = 1),
            selectInput("area", "Area", choices = c('All', unique(pd_c$area))),
            checkboxInput("legend", "Show legend", TRUE)
            # , textOutput("debug")
      )
      
      
      
)


# All Computations Here
server <- function(input, output) {
      
      filteredpd_c <- reactive({pd_c %>% filter(if(input$area == 'All') TRUE else area == input$area)%>% filter(AVG_occupancy>=input$AVG_car[1], AVG_occupancy<=input$AVG_car[2])})
  
  
      output$seattleMap <- renderLeaflet(
            leaflet(filteredpd_c()) %>% 
            addTiles() %>% 
            setView(lat = 47.6225, lng = -122.326771, 14) %>%
            addCircleMarkers(lng = ~long, lat = ~lat, radius = ~AVG_occupancy, opacity = 0.5, fillColor = , popup = ~popup_info)
            )
      
      # observe(
      #       pal <- col_pal(),
      #       leafletProxy(
      #             "seattleMap", 
      #             pd_c = pd_f()) %>% 
      #             clearShapes() %>% 
      #             addCircleMarkers(radius = ~AVG_paidoccupancy, weight = 1, color = ~pal(AVG_paidoccupancy), fillOpacity = 0.7, popup = ~paste(popup_info))
      # )
      
      output$debug <- renderText(paste(input$AVG_car, input$area, typeof(input$area), input$legend, typeof(input$legend)))
}

# Run the application 
shinyApp(ui = ui, server = server)