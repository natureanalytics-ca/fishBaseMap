


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
   p("Fish images are presented as links to the citizen scientist webpage, iNaturalist.org. Detailed information on each image can be obtained by following the links provided. Images only include those with CC license, excluding NC designation."),
   h6("References:"),
   tags$ul(
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
          dropdownMenu = boxDropdown(
            icon = icon("info", "fa-1x"),
            div(
              style = "width: 300px; padding: 20px",
              h6(strong("Environment")),
              p("Filter according to whether species of interest occur in freshwater, brackish or marine environment(s) at any stage of development.")
            )
          ),
          awesomeCheckboxGroup(
            inputId = "fishEnviro",
            label = "",
            choices = c("saltwater", "brackish",  "freshwater"),
            selected = c("saltwater", "brackish",  "freshwater"),
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
          dropdownMenu = boxDropdown(
            icon = icon("info", "fa-1x"),
            div(
              style = "width: 300px; padding: 20px",
              h6(strong("Endemism")),
              p(strong("Endemic"), "- Native and restricted to a particular area."),
              p(strong("Native"), "- Species that occur naturally in a given area or region."),
              p(strong("Introduced"), "- Not native to a particular country but has been brought in by man."),
              p(strong("Reintroduced"), "- Brought into the country after initial introductions failed or after extinction of native species."),
              p("Definitions obtained from FishBase glossary.")
             
            )
          ),
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
          dropdownMenu = boxDropdown(
            icon = icon("info", "fa-1x"),
            div(
              style = "width: 300px; padding: 20px",
              h6(strong("Fish families")),
              p("Narrow results by selecting one or more fish families.")
            )
          ),
          withSpinner(uiOutput("familyOut"))
        )
      ),
      br(),
      fluidRow(
        box(
          title = "Click any row to view available images.",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          background = "primary",
          status = "primary",
          gradient = TRUE,
          withSpinner(uiOutput("species_table"))
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
