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
library(shinyjs)



hoteldata <- read_csv("../data/hotelreview_clean.csv")
hoteldata$link <- paste0("<a href='",hoteldata$link, "'  target='_blank',>",'Book "',hoteldata$hotelname, '" at Booking.com',"</a>")
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
  
  menuItem("About", tabName = "about", icon = icon("user-friends")),
  
  sliderInput("slider1","Radius distance to Nashville Software School:",1,30,10),
  sliderInput("slider2", "Nightly Advertised Price:", 50, 300, 150)
))

## Body content
body <- dashboardBody(tabItems(
  # First tab content
  tabItem(tabName = "dashboard",
          fluidRow(
            box(width = NULL, solidHeader = TRUE,
                actionButton(inputId='ab1', label="Project GitHub", icon = icon("github"), onclick ="window.open('https://github.com/Tingting0618/nss_midterm', '_blank')"),
                actionButton(inputId='ab2', label="Learn Data Science", icon = icon("graduation-cap"), onclick ="window.open('http://nashvillesoftwareschool.com/programs/data-science', '_blank')"),
                actionButton(inputId='ab3', label="Get More Hotel Data", icon = icon("bed"), onclick ="window.open('https://str.com')"),
                
                leafletOutput("mymap"),
                actionButton(inputId='ab4', label="Refresh Now",  icon = icon("refresh"), onclick ="window.location.reload();")
                
               
                ),
            h4("Double-click to clear selection."),
            box(width = NULL,plotlyOutput("plot1"), tags$head(tags$script(src="clickhandler.js")),
            br(),
            br(),
            h4("You may search recommended hotel based on your own preference:"),
            box(sliderInput("dist","Radius distance importance:",1,100,50),
                sliderInput("price","Price importance:",1,100,50)),
            box(sliderInput("score","Review score importance:",1,100,50),
                sliderInput("count","Review count importance:",1,100,50))
                
                )
            
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
    p("Hotel Search Now was developed by Tingting Duan, with tremendous help from her instructor at",
      a(href='http://nashvillesoftwareschool.com/', target="_blank",'Nashville Software School'),
      "Michael Holloway, who is a brilliant mathematician and data scientist.")

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
      filter(Nightly_Price < input$slider2) %>%
      mutate(Score= 30*percent_rank(Review_Count)+ 50*percent_rank(Review_Score)+20*percent_rank(1/(Radius_Dist_to_NSS)**2)+10*percent_rank(1/Nightly_Price)
             ,Score= round(Score, 1))%>% 
      mutate(Recommend = ifelse( Score> 80, "Recommended Hotel", "The Rest Hotels"))
    
    key <- row.names(hotel_data_w_filter)
    
    p <- hotel_data_w_filter %>%
     ggplot(aes(x = Review_Count, y = Review_Score,label = Hotel_Name, key = key,color = Recommend)) + geom_point()
    ggplotly(p) %>% layout(dragmode = "select")
    
  })
  
  #Data Table
  output$table = DT::renderDataTable({
    hoteldata %>%
      filter(Radius_Dist_to_NSS < input$slider1) %>%
      filter(Nightly_Price < input$slider2) %>%
      mutate(Score= 30*percent_rank(Review_Count)+ 50*percent_rank(Review_Score)+20*percent_rank(1/(Radius_Dist_to_NSS)**2)+10*percent_rank(1/Nightly_Price)
             ,Score= round(Score, 1))%>% 
      mutate(Recommend = ifelse( Score> 80, "YES", "NO"))%>% 
      print()
  }	,escape = FALSE)
  
  #Maps
  output$mymap <- renderLeaflet({
    # Get subset based on selection

    hotel_data_w_filter <- hoteldata %>%
      filter(Radius_Dist_to_NSS < input$slider1) %>%
      filter(Nightly_Price < input$slider2)
    
    if (!is.null(event_data("plotly_selected"))) {
      hotel_data_w_filter<- hotel_data_w_filter[event_data("plotly_selected")$key,] }

    
    leaflet(data =hotel_data_w_filter) %>%
      addTiles() %>%
      #      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addMarkers(
        ~ lng,
        ~ lat,
        popup = ~ as.character(Link),
        label = ~ as.character(Hotel_Name)
      )
  })
  

  
}

shinyApp(ui, server)
