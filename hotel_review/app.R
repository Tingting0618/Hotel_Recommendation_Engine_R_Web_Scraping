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
hoteldata$link <- paste0("<a href='",hoteldata$link,"'>",hoteldata$link,"</a>")
hoteldata$radius_dist_to_nss<-round(hoteldata$radius_dist_to_nss, 1)
hoteldata <- hoteldata %>% rename(Hotel_Name = hotelname,
                                  Review_Count = review_num,
                                  Review_Score=review_score,
                                  Radius_Dist_to_NSS=radius_dist_to_nss,
                                  Nightly_Price=nightly_advertised_price,
                                  Link = link)

## Sidebar content
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem("See Results", tabName = "table", icon = icon("list-alt")),
  
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
            box(width = NULL,plotlyOutput("plot1"), tags$head(tags$script(src="clickhandler.js")) )
            
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
    fluidPage( 
    h3("About Hotel Search Now"),
    p(
      "Thanks for visiting our website. Hotel Search Now is an independent, non-commercial set of tools and data that allows you to explore and find the most ideal hotel by leveraging hotel reviews."), 
               
    h3("Disclaimers"),
    p("
        This site is not associated with or endorsed by Booking.com or any of Booking.com's competitors.
        The data utilizes public information compiled from the Booking.com website. Data is verified, cleansed, analyzed and aggregated.
        No private information is being used.
        Accuracy of the information compiled from the Booking.com site is not the responsibility of Hotel Search Now."),
    h3("About Author"),
    p("Hotel Search Now was developed by Tingting Duan, with tremendous help from her instructor at",a(href='http://nashvillesoftwareschool.com/', target="_blank",'Nashville Software School'),"Michael Holloway, who is a mathematician and data scientist.")

  )
  
))
)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Hotel Search Now"),
  sidebar = sidebar,
  body = body
)




server <- function(input, output, session) {
  #BCG Scatter plot
  output$plot1 <- renderPlotly({
    hotel_data_w_filter <- hoteldata %>%
      filter(Radius_Dist_to_NSS < input$slider1) %>%
      filter(Nightly_Price < input$slider2)
    p <- hotel_data_w_filter %>%
     ggplot(aes(x = Review_Count, y = Review_Score,label = Hotel_Name)) + geom_point()#+theme_classic()
    
  })
  
  #Data Table
  output$table = DT::renderDataTable({
    hoteldata %>%
      filter(Radius_Dist_to_NSS < input$slider1) %>%
      filter(Nightly_Price < input$slider2) %>%
      print()
  }	,escape = FALSE
)
  
  #Maps
  output$mymap <- renderLeaflet({
    hotel_data_w_filter <- hoteldata %>%
      filter(Radius_Dist_to_NSS < input$slider1) %>%
      filter(Nightly_Price < input$slider2)
    
    leaflet(data = hotel_data_w_filter) %>%
      addTiles() %>%
      #      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addMarkers(
        ~ lng,
        ~ lat,
        popup = ~ as.character(Link),
        label = ~ as.character(Hotel_Name)
      )
    #     addCircleMarkers(lng =hotel_data_w_filter$lng, 
    #                      lat = hotel_data_w_filter$lat, 
    #                      radius = log(hotel_data_w_filter$Nightly_Price), 
    #                      label = hotel_data_w_filter$Hotel_Name, weight = 10)
  })
  
}

shinyApp(ui, server)