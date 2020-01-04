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
library(plotly)
library(DT)

hoteldata <- read_csv("../data/hotelreview_clean.csv")


## Sidebar content
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem("Get Data", tabName = "table", icon = icon("list-alt")),
  
  menuItem("About", tabName = "about", icon = icon("globe")),
  
  sliderInput("slider1","Radius distance to Nashville Software School:",1,30,10),
  sliderInput("slider2", "Nightly Advertised Price:", 50, 300, 150)
))

## Body content
body <- dashboardBody(tabItems(
  # First tab content
  tabItem(tabName = "dashboard",
          fluidRow(
                   box(width = NULL, solidHeader = TRUE,leafletOutput("mymap"),actionButton("refresh", "Refresh Now")),
                   box(width = NULL,plotlyOutput("plot1"))
                   
                   )
            ),
  
  
  # Second tab content
  tabItem(
    tabName = "table",
    fluidRow(
      column(
        DT::dataTableOutput("table"), width = 6)
    )
  ),

  
  # Third tab content
  tabItem(
    tabName = "about",
    h2(
      "Hi there, Thanks for visiting our website. This is a free tool to help our users to find the most ideal hotel by researching hotel reviews. (This is a student practice project, and comes with no warranty.)"
    )
  )
  
))

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Nashville Hotels"),
  sidebar = sidebar,
  body = body
)




server <- function(input, output, session) {
  #BCG Scatter plot
  output$plot1 <- renderPlotly({
    hotel_data_w_filter <- hoteldata %>%
      filter(radius_dist_to_nss < input$slider1) %>%
      filter(nightly_advertised_price < input$slider2)
    p <- hotel_data_w_filter %>%
      ggplot(aes(x = review_num, y = review_score)) + geom_point() #+theme_classic()
    ggplotly(p)
  })
  
  #Data Table
  output$table = DT::renderDataTable({
    hoteldata %>%
      filter(radius_dist_to_nss < input$slider1) %>%
      filter(nightly_advertised_price < input$slider2) %>%
      print()
  })
  
  #Maps
  output$mymap <- renderLeaflet({
    hotel_data_w_filter <- hoteldata %>%
      filter(radius_dist_to_nss < input$slider1) %>%
      filter(nightly_advertised_price < input$slider2)
    
    leaflet(data = hotel_data_w_filter) %>%
      addTiles() %>%
      #      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addMarkers(
         ~ lng,
         ~ lat,
         popup = ~ as.character(link),
         label = ~ as.character(hotelname)
       )
#     addCircleMarkers(lng =hotel_data_w_filter$lng, 
#                      lat = hotel_data_w_filter$lat, 
#                      radius = log(hotel_data_w_filter$nightly_advertised_price), 
#                      label = hotel_data_w_filter$hotelname, weight = 10)
  })
  
}

shinyApp(ui, server)