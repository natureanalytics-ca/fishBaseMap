##Shape Click Event Leaflet Map in Shiny
## Zoom in to the country and display corresponding click data when clicked

## Source of shape file
# http://thematicmapping.org/downloads/world_borders.php
## Set working directory and Download the shape files 

#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="TM_WORLD_BORDERS_SIMPL-0.3.zip")
## Unzip doownloaded file
#unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")


## Load Required Packages## 
library(leaflet)
library(rgdal) # R 'Geospatial' Data Abstraction Library. 
library(shiny)
library(shinyWidgets)
library(rfishbase)
library(dplyr)
library(DT)
library(shinyalert)

## Load the shape file to a Spatial Polygon Data Frame (SPDF) using the readOGR() function
myspdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")

#Load fishbase country info
fish_master<-country()

## Defining UI 
ui <- fluidPage(
  
  useShinyalert(),

  leafletOutput("mymap"),
  br(),
  DTOutput("map_species_table"),
  # Absolute panel will house the user input widgets
  # Div tag is used to add styling to absolute panel
  absolutePanel(top = 10, right = 10, fixed = FALSE,
                tags$div(style = "opacity: 0.70; background: #FFFFFF; padding: 8px; ",
                         helpText("Welcome to the World map!"),
                         textOutput("map_text") # display the country name in absolute panel when shape is clicked
                )
  )
) ## UI




## R Server side
server <- function(input, output, session) {
  
  #Mapping
  output$mymap <- renderLeaflet({
    # Create the map data and add polygons 
    leaflet(data=myspdf) %>% 
      addTiles() %>% 
      setView(lat=10, lng=0, zoom=2.4) %>%
      addPolygons(fillColor = "green",
                  highlight = highlightOptions(weight = 5,
                                               color = "blue",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = ~NAME,
                  layerId = ~NAME) # add a layer ID to each shape. This will be used to identify the shape clicked
    
  })
  
  # Zoom and set the view after click on state shape
  # input$mymap_shape_click will be NULL when not clicked initially to any shape
  # input$mymap_shape_click will have the ID, lat and lng corresponding to the shape clicked
  # Observe to update the view and zoom level when a shape is clicked
  observe(
    {  click = input$mymap_shape_click
    #  subset the spdf object to get the lat, lng and country name of the selected shape (Country in this case)
    sub = myspdf[myspdf$NAME==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    country = sub$NAME
    if(is.null(click)) ## if nothing is clicked
      return()
    else
      leafletProxy("mymap") %>%
      setView(lng = lng , lat = lat, zoom = 4) %>%
      clearMarkers() %>%
      addMarkers(lng =lng , lat = lat, popup = country)
    # using lat long from spdf will not change the view on multiple clicks on the same shape
    
    }
  )
  
  ## absolute Panel displaying country Name
  output$map_text <- renderText({
    sub = myspdf[myspdf$NAME==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    country <- sub$NAME
    paste("Country:", country)
  })
  
  map_species_list<-reactive({
    sub = myspdf[myspdf$NAME==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    countryIn = sub$NAME
    #Manual changes so that country names from map conform to fishbase country names
    if(length(countryIn) > 0){
      if(countryIn == "United States") countryIn<-"USA"
      if(countryIn == "United Kingdom") countryIn <-"UK"
      if(countryIn == "Marshall islands") countryIn <- "Marshall Is."
      if(countryIn == "Northern Mariana islands") countryIn <- "North Marianas"
    }
    Y<-fish_master %>%
      filter(
        country %in% countryIn,
        Status %in% c("endemic", "native", "introduced", "reintroduced")
      )
    Species<-sort(Y$Species)
    FBname<-species(Species, fields=c("FBname", "PicPreferredName"))
    data.frame(list(Species=Species, FBname=FBname$FBname, Pic=FBname$PicPreferredName))
  })
  
  output$map_species_table <- renderDT({
    datatable(map_species_list()[,1:2],
              colnames = c("Scientific name", "FishBase common name"),
              rownames = FALSE,
              selection = "single",
              options = list(pageLength = 15,
                             scrollX = TRUE,
                             columnDefs = list(list(className = 'dt-left', targets = 0:1)))
    )
    
  })
  map_species_table_Proxy<-dataTableProxy(session$ns('map_species_table'))
  
  observeEvent(input$map_species_table_rows_selected,{
    shinyalert(
      title = map_species_list()$FBname[input$map_species_table_rows_selected],
      imageUrl = paste0("https://www.fishbase.de/images/species/", map_species_list()$Pic[input$map_species_table_rows_selected]),
      imageWidth = 300,
      imageHeight = 300,
    )
  })
}

shinyApp(ui, server)