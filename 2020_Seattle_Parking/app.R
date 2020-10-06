library(shiny)
library(leaflet)
library(dplyr)
library(shinyWidgets)

#Importing Data
parking_data <- jsonlite::fromJSON("https://raw.githubusercontent.com/YuchenS1/StatsBlog/master/content/post/raw/parking_raw_data.json?token=AIK5BGEVNKOAM5JULY2G5Y27QCOHG")
pd_c <- parking_data %>% mutate(date = as.Date(date), AVG_paidoccupancy = round(as.numeric(AVG_paidoccupancy),2), long = location$long, lat = location$lat, woy = as.numeric(woy)) %>% select(-location) %>% mutate(popup_info = paste(blockfacename,"<br/>", AVG_paidoccupancy, "cars on average"))

slider_choice <- format(seq.Date(from = min(pd_c$date + 7), by = 'week', length.out = max(pd_c$woy)), "Week %U")

ui <- fluidPage(
  titlePanel("Seattle Paid Parking 2020"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderTextInput(
        inputId = "timeline", label = "Date", width = "70%",
        choices = slider_choice, 
        grid = FALSE
      )
    ),
    
    mainPanel(
      leafletOutput("seattleMap"),
      textOutput("debug")
    )
  )
)


# Define server logic required
server <- function(input, output) {
  pd_f <- reactive({pd_c %>% filter(woy == as.numeric(substr(as.character(input$timeline), 6,7)))})
  
  col_pal <- reactive({
    colorNumeric("magma", pd_f$AVG_paidoccupancy)
  })
  
  output$seattleMap <- renderLeaflet(
    leaflet() %>% addTiles() %>% setView(lat = 47.6225, lng = -122.326771, 14))
    
  observe(
    pal <- col_pal(),
    leafletProxy("seattleMap", data = pd_f()) %>% clearShapes() %>% addCircleMarkers(radius = ~AVG_paidoccupancy, weight = 1, color = ~pal(AVG_paidoccupancy), fillOpacity = 0.7, popup = ~paste(popup_info))
  )

  # output$debug <- renderText(as.numeric(substr(as.character(input$timeline), 6,7)) + 1)
}

# Run the application 
shinyApp(ui = ui, server = server)