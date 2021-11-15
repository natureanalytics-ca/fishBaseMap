


#----------------------------
# UI
#----------------------------

header <- dashboardHeader(
  title = dashboardBrand(
    title = "Fish Geo",
    color = "primary",
    href = "http://natureanalytics.ca",
    image = "NA.png",
  )
  
)

sidebar <- dashboardSidebar(
  minified =FALSE,
  sidebarMenu(
    menuItem(
      tabName = "mappingTool",
      text = "World map"
    )
  )
)

controlbar<-dashboardControlbar(
 title = "Notice to users",
 width = 600,
 div(
   style = "padding: 20px;",
   h6("Fishbase"),
   p("Fish life history information, including geography, naming conventions and families are obtained from fishbase using 
   the rfishbase library. Conventions such as naming and endemism may differ from other authorities."),
   h6("References:"),
   tags$ul(
     tags$li(tags$a(href="https://www.fishbase.de", "Fishbase.org", target="_blank")),
     tags$li(tags$a(href="https://www.fishbase.de/manual/english/fishbasethe_species_table.htm", "Fishbase country table", target="_blank")),
     tags$li(tags$a(href="https://www.fishbase.de/manual/english/fishbasethe_species_table.htm", "Fishbase species table", target="_blank")),
     tags$li(tags$a(href="hhttps://cran.r-project.org/web/packages/rfishbase/index.html", "rfishbase library", target="_blank"))
   ),
   br(),
   h6("iNaturalist"),
   p("Fish images are presented as links to the citizen scientist webpage, iNaturalist.org. Detailed information on each image can be obtained by following the links provided."),
   h6("References:"),
   tags$ul(
     tags$li("Images are limited to those with CC license, excluding those with NC designation."),
     tags$li(tags$a(href="https://cran.r-project.org/web/packages/rinat/index.html", "rinat library", target="_blank"))
     
   )
 )
)


body<-dashboardBody(
  
  
  #---------------
  #Call to
  #---------------
  useShinyjs(),
  useWaiter(),
  waiterPreloader(
    html = tagList(
      spin_loaders(id=19, color = "white"),
      h5("Loading world map...")
    ),
    fadeout = TRUE
  ),
  
  #----------------------
  #Read styling items
  #----------------------
  use_theme(inputTheme),
  includeCSS("www/main.css"),

  tabItems(
    
    #---------------------------
    #Boxes
    #---------------------------
    
    tabItem(
      tabName = "mappingTool",
      leafletOutput("mymap"),
      br(),
      fluidRow(
        valueBoxOutput("saltBox"),
        valueBoxOutput("brackishBox"),
        valueBoxOutput("freshBox")
      ),
      br(),
      fluidRow(
        box(
          title = "Filter by environment",
          status = "primary",
          width = 4,
          height = "360px",
          collapsible = FALSE,
          solidHeader = TRUE,
          background = "primary",
          gradient = TRUE,
          prettyRadioButtons(
            inputId = "fishEnviro",
            label = "",
            choices = c("saltwater", "brackish",  "freshwater"),
            status = "default"
          )
        ),
        box(
          title = "Filter by endemism",
          status = "primary",
          width = 4,
          height = "360px",
          collapsible = FALSE,
          solidHeader = TRUE,
          background = "primary",
          gradient = TRUE,
          awesomeCheckboxGroup(
            inputId = "fishEndem",
            label = "",
            choices = c("endemic", "native", "introduced", "reintroduced"),
            selected = c("endemic", "native", "introduced", "reintroduced"),
            status = "default"
          )
        ),
        box(
          title = "Filter by family",
          status = "primary",
          width = 4,
          height = "360px",
          collapsible = FALSE,
          solidHeader = TRUE,
          background = "primary",
          gradient = TRUE,
          withSpinner(uiOutput("familyOut"))
        )
      ),
      br(),
      fluidRow(
        box(
          title = "Click on any row to view images of the selected species.",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          background = "primary",
          gradient = TRUE,
          withSpinner(DTOutput("species_table"))
        )
        
      ),

      #-------------------------------------------------
      # Absolute panel will house the user input widgets
      #-------------------------------------------------
      absolutePanel(top = 100, right = 50, fixed = FALSE,
                    tags$div(style = "opacity: 0.80; background-color: #000000; padding: 8px; color: #FFFFFF; ",
                             helpText("Select a country to create a fish species list."),
                             textOutput("map_text") # display the country name in absolute panel when shape is clicked
                    )
      )
    )
  )
)

dashboardPage(
  dark =TRUE,
  header = header,
  sidebar = sidebar,
  body = body,
  controlbar = controlbar,
  footer = footer
)
