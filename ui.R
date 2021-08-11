
## Load Required Packages## 
library(shiny)
#library(shinydashboard)
library(fresh)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(rgdal) # R 'Geospatial' Data Abstraction Library. 
library(shinyWidgets)
library(rfishbase)
library(dplyr)
library(DT)
library(shinyalert)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(rinat)

#---------------------------------------------------------------------------------
# Setup theme (fresh library)
#
# Relies on library fresh. 
# Manual setup required. 
# Does not take bootstrap buit-in themes
# NULL will retain default value
#--------------------------------------------------------------------------------


inputTheme <- create_theme(
  
  #Change meaning of built in colors
  adminlte_color(
    light_blue = "#58C7B1",
    red = NULL,
    green = NULL,
    aqua = NULL,
    yellow = NULL,
    blue = NULL,
    navy = NULL,
    teal = NULL,
    olive = NULL,
    lime = NULL,
    orange = NULL,
    fuchsia = NULL,
    purple = NULL,
    maroon = NULL,
    black = NULL,
    gray_lte = NULL
  ),
  
  #Sidebar
  adminlte_sidebar(
    width = NULL,
    dark_bg = NULL,
    dark_hover_bg = NULL,
    dark_color = NULL,
    dark_hover_color = NULL,
    dark_submenu_bg = NULL,
    dark_submenu_color = NULL,
    dark_submenu_hover_color = NULL,
    light_bg = NULL,
    light_hover_bg = NULL,
    light_color = NULL,
    light_hover_color = NULL,
    light_submenu_bg = NULL,
    light_submenu_color = NULL,
    light_submenu_hover_color = NULL
  ),
  
  #Global options
  adminlte_global(
    content_bg = NULL,
    box_bg = NULL,
    info_box_bg = NULL
  ),
  
  #not necessary to write to file
  output_file = NULL
)


#-----------------------------------------
#Other layout and styling formatting, HTML
#-----------------------------------------
inputStyle<-tags$style(
  
  #DT selected row color
  HTML('table.dataTable tr.selected td, table.dataTable td.selected {
    background-color: #58C7B1 !important;}'
  ),
  
  #DT scrollX
  HTML(".dataTables_wrapper { overflow-x: scroll; }" ),
  
  #Hyperlink color
  HTML("a {color: black ;}"),
  
  #ShinyWidgets progress bar
  HTML(".progress-bar{background-color: #58C7B1;}"),
  
  #Carousel
   HTML(".carousel-control {
    display: none;
  }"),
  HTML(".carousel-indicators li { 
    display: inline-block;
    width: 12px;
    height: 12px;
    margin: -20px;
    text-indent: 0;
    cursor: pointer;
    border: none;
    border-radius: 50%;
    background-color: lightgrey;
  }"),
  HTML(" .carousel-indicators .active {
    width: 18px;
    height: 18px;
    margin: -20px;
    background-color: dodgerblue;
  }"),
  HTML(".carousel { height:400px;}"),

  #shinyBS popover width
  HTML(".popover{max-width:50%;}")
)


#----------------------------------------
#Footer - for NA copyright, terms of use
#----------------------------------------
footer = dashboardFooter(
  right = div(fluidRow(tags$a(href="https://natureanalytics.ca/terms","Terms of use", 
                              target="_blank")
              )
  ),
  left = tags$a(href="https://natureanalytics.ca", 
                tags$image(src="black_logo_transparent_background.png", 
                           height=60, 
                           width=128
                ),  
                target="_blank")
)

#----------------------------
# UI
#----------------------------

header <- dashboardHeader(
  title = div(align="left", "Fish geo")
)

sidebar <- dashboardSidebar(
  #minified = FALSE,
  sidebarMenu(
    menuItem(
      tabName = "mappingTool",
      text = "World map"
    )
  )
)


body<-dashboardBody(
  
  useShinyjs(),
  useShinyalert(),
  
  #----------------------
  #Read styling items
  #----------------------
  use_theme(inputTheme),
  inputStyle,

  tabItems(
    
    #---------------------------
    #Boxes
    #---------------------------
    
    tabItem(
      tabName = "mappingTool",
      br(),
      h4("Select a country to generate its corresponding fish species list."),
      withSpinner(leafletOutput("mymap")),
      br(),
      br(),
      radioButtons(
        inputId = "fishEnviro",
        label = "Filter by environment",
        choices = c("Saltwater", "Freshwater", "Any environment")
      ),
      br(),
      h4("Select a row from the table to view images of the selected species."),
      br(),
      withSpinner(DTOutput("species_table")),
      # Absolute panel will house the user input widgets
      # Div tag is used to add styling to absolute panel
      absolutePanel(top = 130, right = 20, fixed = FALSE,
                    tags$div(style = "opacity: 0.70; background: #FFFFFF; padding: 8px; ",
                             helpText("Welcome to the World map!"),
                             textOutput("map_text") # display the country name in absolute panel when shape is clicked
                    )
      )
    )
  )
)


shinydashboardPlus::dashboardPage(
  skin = "black",
  header = header,
  sidebar = sidebar,
  body = body,
  footer = footer
)
