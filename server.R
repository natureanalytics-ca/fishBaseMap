

#-----------------------------
#Server
#-----------------------------

#------------------------------------
# Loading map data and fishbase data
#-----------------------------------

##Shape Click Event Leaflet Map in Shiny
## Zoom in to the country and display corresponding click data when clicked

## Source of shape file
# http://thematicmapping.org/downloads/world_borders.php
## Set working directory and Download the shape files 

#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="TM_WORLD_BORDERS_SIMPL-0.3.zip")
## Unzip doownloaded file
#unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")


## Load the shape file to a Spatial Polygon Data Frame (SPDF) using the readOGR() function
myspdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")

#Load fishbase country info
# fish_master<-country()
# fish_master<-fish_master %>%
#   select("country", "Status", "Species")
# saveRDS(fish_master, file = "fishbase_countries.rds")
#fish_master<-readRDS( file = paste0(getwd(), "/fishbase_countries.rds"))
Sys.setenv(FISHBASE_HOME=paste0(getwd(), "/www"))
fish_master<-country()

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
    FBname<-species(Species, fields=c("Species", "FBname", "Fresh", "Brack", "Saltwater"))
    return(FBname)
  })
  
  plot_species_list<-reactive({
    req(input$fishEnviro)
    if(input$fishEnviro == "Saltwater") {
      dtIn<-map_species_list()%>%
        filter(Saltwater == -1)  %>%
        mutate(Enviro = rep("Saltwater", NROW(Saltwater))) %>%
        select("Species", "FBname", "Enviro")
    }
    
    if(input$fishEnviro == "Freshwater") {
      dtIn<-map_species_list()%>%
        filter(Fresh == -1) %>%
        mutate(Enviro = rep("Freshwater", NROW(Fresh))) %>%
        select("Species", "FBname", "Enviro")
    }
    
    if(input$fishEnviro == "Any environment") {
      dtIn<-map_species_list() %>%
        mutate(Enviro = rep("Any", NROW(Saltwater))) %>%
        select("Species", "FBname", "Enviro")
    }
    dtIn
  })
  
  output$species_table <- renderDT({
    
    datatable(plot_species_list(),
              colnames = c("Scientific name", "FishBase common name", "Environment"),
              rownames = FALSE,
              selection = "single",
              options = list(pageLength = 15,
                             scrollX = TRUE,
                             columnDefs = list(list(className = 'dt-left', targets = 0:1)))
    )
    
  })
  
  species_table_Proxy<-dataTableProxy(session$ns('species_table'))
  
  observeEvent(input$species_table_rows_selected,{
    
    show_condition <- function(code) {
      tryCatch(code,
               error = function(c) NULL,
               warning = function(c) NULL
      )
    }
    
    #inaturalist
    INname<-show_condition(get_inat_obs(taxon_name = plot_species_list()$Species[input$species_table_rows_selected], maxresults = 10))
    
    if(NROW(INname) > 0) {
      
      INname<-INname %>%
        filter(license != "") %>%
        filter(image_url != "") %>%
        mutate(uselic = str_detect(license, "CC"))
      
      if (NROW(INname) > 0) {
        
        mxImg<-ifelse(NROW(INname) > 5, 5, NROW(INname))
        item<-carouselEdit(
          id = "fishCarousel",
          indicators = TRUE,
          width = 12,
          .list =
            lapply(1:mxImg, function(i) {
              src = INname$image_url[i]
              carouselItem(
                  div(
                    align ="center",
                    tags$image(src = src, style="height: 300px; align: center"),
                    tags$a(href=INname$url[i],
                           h5("Click for photo credit on iNaturalist.org"),
                           target="_blank")
                  )
              )
            })
        )
      } else {
        item<-h5("No images found")
      }
    } else {
      item<-h5("No images found")
    }
    
    shinyalert(
      html = TRUE,
      closeOnClickOutside = FALSE,
      size = "s",
      immediate = TRUE,
      text = tagList(
        h3(plot_species_list()$FBname[input$species_table_rows_selected]),
        h3(plot_species_list()$Species[input$species_table_rows_selected]),
        item
        )
    )
  })
  
  #----------------------------------------------------------------------------------------
  #Modification of the shinydashboard carousel funtion that prevents auto-advance of slides
  #----------------------------------------------------------------------------------------
  
  carouselEdit <- function (...,
                            id,
                            indicators = TRUE,
                            width = 6,
                            .list = NULL)
  {
    items <- c(list(...), .list)
    generateCarouselNav <- function(items) {
      found_active <- FALSE
      navs <- lapply(seq_along(items), FUN = function(i) {
        active <- if (found_active) {
          FALSE
        } else {
          sum(grep(x = items[[i]]$attribs$class, pattern = "active")) == 1
        }
        if (active && !found_active) found_active <- TRUE
        shiny::tags$li(
          `data-target` = paste0("#",id),
          `data-slide-to` = i - 1,
          class = if (active) "active"
        )
      }
      )
      # actives <- dropNulls(lapply(navs, function(nav) {
      #   nav$attribs$class
      # }))
      # if (length(actives) == 0) {
      navs[[1]]$attribs$class <- "active"
      items[[1]]$attribs$class <<-
        paste0(items[[1]]$attribs$class,
               " active")
      # }
      navs
    }
    indicatorsTag <-
      shiny::tags$ol(class = "carousel-indicators",
                     generateCarouselNav(items))
    bodyTag <- shiny::tags$div(class = "carousel-inner",
                               items)
    controlButtons <- if (indicators) {
      shiny::tagList(
        shiny::tags$a(
          class = "left carousel-control",
          href = paste0("#", id),
          `data-slide` = "prev",
          shiny::tags$span(class = "fa fa-angle-left")
        ),
        shiny::tags$a(
          class = "right carousel-control",
          href = paste0("#", id),
          `data-slide` = "next",
          shiny::tags$span(class = "fa fa-angle-right")
        )
      )
    }
    else {
      NULL
    }
    carouselTag <-
      shiny::tags$div(
        class = "carousel slide",
        `data-interval` = 'false',
        `data-ride` = "carousel",
        `data-pause` = 'hover',
        id = id
      )
    carouselTag <-
      shiny::tagAppendChildren(carouselTag, indicatorsTag,
                               bodyTag, controlButtons)
    shiny::tags$div(class = if (!is.null(width))
      paste0("col-sm-", width), carouselTag)
  }
}

