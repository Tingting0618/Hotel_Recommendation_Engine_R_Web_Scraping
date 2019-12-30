#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)

hoteldata <- read_csv("../data/hotelreview.csv")


## Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Map", tabName = "map", icon = icon("globe"))
  )
)

## Body content
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Filter1",
                sliderInput("slider1", "Radius distance to Nashville Software School:", 1, 100, 50)
              ),
              
              box(
                title = "Filter2",
                sliderInput("slider2", "Nightly Advertised Price:", 50, 300, 150)
              ),
              
              tableOutput("table")
            )
    ),
    
    # Second tab content
    tabItem(tabName = "map",
            #h2("not yet sure how to insert a map"),
            leafletOutput("mymap"),
            p()
            #actionButton("recalc", "New points")
    )
  )
)

ui <- dashboardPage(
  skin="black",
  dashboardHeader(title = "Basic dashboard"),
  sidebar = sidebar,
  body = body

  )




server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
  hotel_data_w_filter<- hoteldata %>% 
      filter(radius_dist_to_nss < input$slider1) %>%
      filter(nightly_advertised_price < input$slider2) 
  hotel_data_w_filter %>% 
      ggplot(aes(x=review_num,y=review_score)) + geom_point() 
    
  })
  
  output$table <- renderTable({
    hoteldata %>% 
      filter(radius_dist_to_nss < input$slider1) %>%
      filter(nightly_advertised_price < input$slider2) %>% 
      print()
  })
  
  #Maps
  output$mymap <- renderLeaflet({
    
    hotel_data_w_filter<- hoteldata %>% 
      filter(radius_dist_to_nss < input$slider1) %>%
      filter(nightly_advertised_price < input$slider2) 
    
    leaflet(data = hotel_data_w_filter) %>% 
      addTiles() %>%
#      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      addMarkers(~lng, ~lat, popup = ~as.character(hotelname), label = ~as.character(hotelname))
  })
    
}

shinyApp(ui, server)