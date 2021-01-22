# libraries

library(shiny)
library(dygraphs)
library(visNetwork)
library(leaflet)
library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)
library(xts)
library(rgdal)
library(lubridate)


# load csv file from:
# https://data.humdata.org/dataset/76d1790e-1303-4284-a245-e9093419e357/resource/ee990c5a-f9c0-418c-bc1c-de4d90b60544/download/odp_contributionsbygender.csv

contrib <- read.csv('odp_contributionsbygender.csv',check.names=FALSE)
contrib <- rename (contrib, names = Contributing_Country)
contrib$Total_Personnel <- contrib$Female_Personnel + contrib$Male_Personnel
contrib$MonthYear <- parse_date_time(contrib$Last_Reporting_Date, "dmy") 
contrib$MonthYear <-  floor_date(contrib$MonthYear, unit="month")
#troops <- subset(contrib, Personnel_Type=='Troops')
#eom <- subset(contrib, Personnel_Type=='Experts on Mission')
#fpu <- subset(contrib, Personnel_Type=='Formed Police Units')
#police <- subset(contrib, Personnel_Type=='Individual Police')
#staff <- subset(contrib, Personnel_Type=='Staff Officer')

worldmap = map("world", fill = TRUE, plot = FALSE)

# UI
ui <- fluidPage(
    titlePanel("UN Peacekeeping Contributors"),
    tabsetPanel(
        tabPanel("Map",
            fluidRow(        
                column(3, selectInput("type", "Personnel Type:", choices = c("Troops", "Experts on Mission", "Formed Police Units", "Individual Police", "Staff Officer"), selected="Troops") #end select input
                       
                      ),#End selector column
                column(9, leafletOutput('map')) # End map column
                    ), # End map row
            # hat tip to https://xjavascript.com/view/505889/shiny-slider-input-step-by-month for the slider
            fluidRow(
                column(12, sliderInput("date", "Report Month", min = as_date(min(contrib$MonthYear)), max = as_date(max(contrib$MonthYear)), value=as.Date("2002-07-01"),timeFormat="%b %Y"),
                textOutput("SliderText")
                       ) # End slider column
                    ), # End time slider row
            fluidRow(
                column(12, plotOutput('barplot'))
                    ) # End bar plot row
                 
#                 ), #End "Map" tab panel
#        
#        tabPanel("Contributor Details",
#                fluidRow(
#                    column(2, selectInput("names", "Contributing Country:", choices = contrib$names),selectInput("Last_Reporting_Date", "Report Date:", choices = contrib$Last_Reporting_Date)), 
#                    column(10, tableOutput('table'))
#                         ) # End fluidRow

                 ) # End "Contributor Details" panel
                       
                ) # End tabsetPanel
                 ) # End fluidPage

# Server
server <- function(input, output) {
    
# initialize reactive variables
        reac <- reactiveValues (date = "30-Nov-20", type = "Troops")
   
########## Tab 1 Functions #####

    output$map = renderLeaflet({
        
        sub <- subset(contrib, MonthYear==as_date(input$date) & Personnel_Type==input$type)
        chorodata <- group_by(sub, names) %>% summarize(Total_Contrib = sum(Total_Personnel))
        pal <- colorNumeric("Blues", domain = chorodata$Total_Contrib)

        map <- leaflet(data = worldmap) %>%
        addTiles() %>%
        setView(35.9375, 14.3754, zoom = 1.5) %>%
        addPolygons(fillColor = ~pal(chorodata$Total_Contrib),weight = 1, opacity =1, color ="white", fillOpacity = 0.7,

                   highlight = highlightOptions(
                       weight = 2,
                       color = "#666",
                       fillOpacity = 0.9,
                       bringToFront = FALSE)
                   )%>%
        leaflet::addLegend("bottomright", pal = pal, values = ~chorodata$Total_Contrib,
    title = "Total Personnel",
    opacity = 1
                  )
        map
    }) # end output map
  
#    output$table =  renderTable ({
 #       myTable <- subset(contrib, Contributing_Country==input$Contributing_Country & Last_Reporting_Date==input$Last_Reporting_Date)
 #       myTable
 #       }) # end output table
       
   # Observe click on country and render bar plot
  observeEvent(input$map_shape_click, {

      # get the geo coordinates of the shape click event
      lon <- input$map_shape_click$lng
      lat <- input$map_shape_click$lat

      # find the country name using the maps package
      cn <- maps::map.where("world", lon, lat)
      # update the reactive variable to trigger changes in the plots
      reac$country <- gsub(":.*", "", cn)  # get everything until : character
  #    output$text <- renderPrint(isolate(reac$state))
   
    # Observe Event triggers bar plot
   output$barplot = renderPlot({
    # subset by names and MonthYear, group_by Personnel_Type, summarize Total_Personnel
       graph_sub <- subset(contrib, names==reac$country & MonthYear==as_date(input$date))
       graph_data <- group_by(graph_sub, Personnel_Type)%>%summarize(Total = sum(Total_Personnel))

     
       p <- ggplot(data=graph_data, aes(x=Personnel_Type, y=Total, color=Personnel_Type))

      p <- p + geom_bar (stat="identity") + ggtitle(paste0(reac$country, " Peacekeepers by Type"))
       
      p
          }) # End bar plot
  }) # End of ObserveEvent
    

} # End server

shinyApp(ui, server)