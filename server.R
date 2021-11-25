

#-----------------------------
#Server
#-----------------------------

server <- function(input, output, session) {
  
  #-------------------------------
  #Mapping
  #-------------------------------
  
  output$mymap <- renderLeaflet({
    leaflet(data=world) %>%
      addTiles() %>%
      setView(lat=10, lng=0, zoom=2.4) %>%
      addPolygons(
        
        stroke = TRUE,
        color = "#78959E",
        weight = 2,
        opacity = 0.5,
        fill = TRUE,
        fillColor = "#DED892",
        fillOpacity = 0.1,
       
                  highlight = highlightOptions(color = "#343a40",
                                               weight = 2,
                                               opacity = 0.8,
                                               fillColor = "#608691",
                                               fillOpacity = 0.5,
                                               bringToFront = TRUE),
                  label = ~name_long,
                  layerId = ~name_long) %>%
      setMapWidgetStyle(list(background= "white"))

  
    
  })
  
  
  #-------------------------------------
  #Update species list on  map click
  #-------------------------------------
  
  map_species_list<-reactiveVal(NULL)
  
  observeEvent(input$mymap_shape_click, {  
  
    waitScreen$show()

    #------------------------------------------
    #Update map by zooming to country selected
    #-----------------------------------------
    
    lat = input$mymap_shape_click$lat
    lng = input$mymap_shape_click$lng
    country = input$mymap_shape_click$id
    leafletProxy("mymap") %>%
      setView(lng = lng , lat = lat, zoom = 4) %>%
      clearMarkers() %>%
      addMarkers(lng =lng , lat = lat, popup = country)
    
    #------------------------------------
    #Get species list
    #----------------------------------
      
    countryIn = input$mymap_shape_click$id
    #Manual changes so that country names from map conform to fishbase country names
    if(length(countryIn) > 0){
      if(countryIn == "United States") countryIn<-"USA"
      if(countryIn == "United Kingdom") countryIn <-"UK"
      if(countryIn == "Marshall islands") countryIn <- "Marshall Is."
      if(countryIn == "Northern Mariana islands") countryIn <- "North Marianas"
      if(countryIn == "Russian Federation") countryIn <- "Russia"
      
    }
    Y<-fish_master %>%
      filter(
        country %in% countryIn,
        Status %in% c("endemic", "native", "introduced", "reintroduced")
      )
    Y<-Y[!duplicated(Y$Species),]
    Y<-Y[order(Y$Species),]
   
    
    X<-species(Y$Species, fields=c("Species", "FBname", "Fresh", "Brack", "Saltwater"))
    X<-X[!duplicated(X$Species),]
    X<-X[order(X$Species),]
    
    Z<-rfishbase::load_taxa() %>% 
      filter(Species %in% local(X$Species)) %>%
      collect()
    Z<-Z[!duplicated(Z$Species),]
    
    map_species_list(X %>%
      left_join(y = Y, by = "Species") %>%
      left_join(y = Z, by = "Species") %>%
      mutate(Family = replace(Family, is.na(Family), "Unknown"))
    )
    
    waitScreen$hide()
  })
  
  #------------------------------------------
  #Absolute Panel displaying country Name
  #-----------------------------------------
  
  output$map_text <- renderText({
    if(!is.null(input$mymap_shape_click)) paste("Country:", input$mymap_shape_click$id)
  })
  
  #------------------------
  # Value boxes
  #------------------------
  
  output$saltBox <- renderValueBox({
    
    if(is.null(map_species_list())){
      saltwater<-0
    } else {
      total<- NROW(map_species_list()%>%
                         filter(Saltwater == -1))
      if("saltwater" %in% input$fishEnviro) {
        saltwater<-NROW(
          map_species_list()%>%
            filter(Saltwater == -1) %>%
            filter(Status %in% input$fishEndem) %>%
            filter(Family %in% input$family)
        )
        saltwater<-paste(saltwater, "of", total)
      } else {
        saltwater<-paste(0, "of", total)
      }
    }   
    valueBox(
      value = saltwater,
      subtitle = "Saltwater species",
      color = "secondary",
      icon = icon("fish")
    )
  })
  
  output$brackishBox <- renderValueBox({
    
    if(is.null(map_species_list())){
      brackish<-0
    } else {
      total<- NROW(map_species_list()%>%
                     filter(Brack == -1))
      if("brackish" %in% input$fishEnviro) {
        brackish<-NROW(
          map_species_list()%>%
            filter(Brack == -1) %>%
            filter(Status %in% input$fishEndem) %>%
            filter(Family %in% input$family)
        )
        brackish<-paste(brackish, "of", total)
      } else {
        brackish<-paste(0, "of", total)
      }
    } 
    
    valueBox(
      value = brackish,
      subtitle = "Brackish species",
      color = "success",
      icon = icon("fish")
    )
  })
  
  output$freshBox <- renderValueBox({
    
    if(is.null(map_species_list())){
      freshwater<-0
    } else {
      total<- NROW(map_species_list()%>%
                     filter(Fresh == -1))
      if("freshwater" %in% input$fishEnviro) {
        freshwater<-NROW(
          map_species_list()%>%
            filter(Fresh == -1) %>%
            filter(Status %in% input$fishEndem) %>%
            filter(Family %in% input$family)
        )
        freshwater<-paste(freshwater, "of", total)
      } else {
        freshwater<-paste(0, "of", total)
      }
    } 
    
    valueBox(
      value = freshwater,
      subtitle = "Freshwater species",
      color = "info",
      icon = icon("fish")
    )
  })
  

  #-----------------------------
  #Family list
  #----------------------------
  
  output[["familyOut"]]<-renderUI({
    req(map_species_list())
    tagList(
    multiInput(
      inputId = "family",
      label = "", 
      choices = unique(map_species_list()$Family),
      selected = unique(map_species_list()$Family),
      options = list(
        enable_search = TRUE,
        non_selected_header = "Families:",
        selected_header = "Selected:"
      )
    ),
    div(style = "display: inlilne;",
        div(style = "float: left;", actionButton("groupSelectNone", "Select none", status = "info")),
        div(style = "float: right;", actionButton("groupSelectAll", "Select all", status = "info"))
    )
    )
  })
  
  #Group selection (select all)
  observeEvent(input$groupSelectAll, {
    updateMultiInput(session, "family", selected = unique(map_species_list()$Family))
  })
  
  #Group selection (select none)
  observeEvent(input$groupSelectNone, {
    updateMultiInput(session, "family", selected = character(0))
  })
  
  #--------------------------
  #Filtering of species list
  #--------------------------
  
  plot_species_list<-reactive({

    req(map_species_list())

    #-----------------
    #Saltwater
    #-----------------
    salt<-NULL
    if("saltwater" %in% input$fishEnviro) {
        salt<-map_species_list() %>%
        filter(Saltwater == -1)  %>%
        mutate(Enviro = rep("Saltwater", NROW(Saltwater))) %>%
        filter(Status %in% input$fishEndem) %>%
        filter(Family %in% input$family) %>%
        select("Species", "FBname", "Family")
    }

    #-----------------
    #Freshwater
    #-----------------
    fresh<-NULL
    if("freshwater" %in% input$fishEnviro) {
      fresh<-map_species_list()%>%
        filter(Fresh == -1) %>%
        mutate(Enviro = rep("Freshwater", NROW(Fresh))) %>%
        filter(Status %in% input$fishEndem) %>%
        filter(Family %in% input$family) %>%
        select("Species", "FBname", "Family")
    }

    #-----------------
    #Brackish
    #-----------------
    brack<-NULL
    if("brackish" %in% input$fishEnviro) {
      brack<-map_species_list() %>%
        filter(Brack == -1) %>%
        mutate(Enviro = rep("Brackish", NROW(Saltwater))) %>%
        filter(Status %in% input$fishEndem) %>%
        filter(Family %in% input$family) %>%
        select("Species", "FBname", "Family")
    }

    dtIn<-rbind(salt, fresh, brack)
    dtIn<-dtIn[!duplicated(dtIn$Species),]
    dtIn
  })

  #--------------------------------
  #Selectable table of species
  #--------------------------------
  
  output$species_table <- renderDT({
    req(plot_species_list())
    datatable(plot_species_list(),
              colnames = c("Scientific name", "Common name", "Family"),
              rownames = FALSE,
              selection = "single",
              options = list(pageLength = 15,
                             scrollX = TRUE,
                             columnDefs = list(list(className = 'dt-left', targets = 0:1)))
    )
  })
  species_table_Proxy<-dataTableProxy(session$ns('species_table'))
  
  
  #---------------------------
  #Image event
  #---------------------------
  
  observeEvent(input$species_table_rows_selected,{
    
    waitScreen$show()
    
    show_condition <- function(code) {
      tryCatch(code,
               error = function(c) NULL,
               warning = function(c) NULL
      )
    }
    
    #inaturalist
    INname<-show_condition(get_inat_obs(taxon_name = plot_species_list()$Species[input$species_table_rows_selected], maxresults = 100))
    
    if(NROW(INname) > 0) {
      
      #Remove any 'non-commerical only' images as a precaution
      INname<-INname %>%
        filter(license != "") %>%
        filter(image_url != "") %>%
        mutate(uselic = str_detect(license, "CC")) %>%
        mutate(containsnc = str_detect(license, "NC")) %>%
        filter(uselic) %>%
        filter(!containsnc)
      
      
      if (NROW(INname) > 0) {
        
        mxImg<-ifelse(NROW(INname) > 10, 10, NROW(INname))
        item<-bs4Carousel(
          id = "fishCarousel",
          indicators = FALSE,
          width = 12,
          .list =
            lapply(1:mxImg, function(i) {
              src = INname$image_url[i]
              bs4CarouselItem(
                
                div(
                  align = "center",
                  tags$image(src = src, style="max-height: 500px; border-radius: 10px;"),
                ),
                caption = div(
                            HTML(
                              ifelse(INname$license[i]=="CC0", paste0("user: ", INname$user_id[i], ", no rights reserved (CC0)."), 
                                     paste0("&copy user: ", INname$user_id[i], ", some rights reserved (", INname$license[i], ")"))
                              
                            ),
                            br(),
                            tags$a(href=INname$url[i], "Find more details at iNaturalist.org", target="_blank"),
                            h6(INname$place_guess[i])
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
    
    waitScreen$hide()
  
    showModal(
      modalDialog(
        easyClose = TRUE,
        size = "l",
        footer = "Images may not correspond to country of interest",
        tagList(
          h3(plot_species_list()$FBname[input$species_table_rows_selected]),
          h4(em(plot_species_list()$Species[input$species_table_rows_selected])),
          br(),
          item,
          br()
        )
      )
    )
  })
  
  #------------------------
  #Download
  #------------------------
  
  #Hide/Show download button
  observe({
    #cond<-NROW(plot_species_list()) > 0
    toggle("downloadData", condition = NROW(plot_species_list()) > 0)
    #  if(cond) {
    #   show(id = "downloadData")
    # } else {
    #   hide(id = "downloadData")
    # }
  })
  
  output$downloadData <- downloadHandler(
 
    filename = function() {
      paste("speciesList-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data<-rbind(
        c("Selected environment:", paste(input$fishEnviro, collapse=" "), ""),
        c("Selected endemism:", paste(input$fishEndem, collapse=" "), ""),
        c(" ", " ", " "),
        names(plot_species_list()),
        plot_species_list()
      )
      write.table(data, file, sep=",", row.names = FALSE, col.names = c(" ", " ", " "))
    }
  )
  
  
  
  
  
  
  
}

