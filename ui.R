


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
   p("Fish images are prsented as links to the citizen scientist webpage, iNaturalist.org. Detailed information on each image can be obtained by following the links provided. Images only include those with CC license, excluding NC designation."),
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
  useShinyjs(),
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
          id = "filterTrait",
          title = "Filter by trait",
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
              style = "width: 350px; padding: 10px",
              h6(strong("Environment")),
              p("Filter according to whether species of interest occur in freshwater, brackish or marine environment(s) at any stage of development."),
              br(),
              h6(strong("Endemism")),
              p(strong("Endemic"), "- Native and restricted to a particular area.", 
                strong("Native"), "- Species that occur naturally in a given area or region.",
                strong("Introduced"), "- Not native to a particular country but has been brought in by man.",
                strong("Reintroduced"), "- Brought into the country after initial introductions failed or after extinction of native species."
              ),
              p("Definitions obtained from FishBase glossary.")
            )
          ),
          awesomeCheckboxGroup(
            inputId = "fishEnviro",
            label = "Filter by environment",
            choices = c("saltwater", "brackish",  "freshwater"),
            selected = c("saltwater", "brackish",  "freshwater"),
            status = "default"
          ),
          br(),
          awesomeCheckboxGroup(
            inputId = "fishEndem",
            label = "Filter by endemism",
            choices = c("endemic", "native", "introduced", "reintroduced"),
            selected = c("endemic", "native", "introduced", "reintroduced"),
            status = "default"
          )
        ),
        box(
          id = "filterFamily",
          title = "Filter by fish family",
          status = "primary",
          width = 8,
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
          #withSpinner(uiOutput("familyOut"))
          #uiOutput("familyOut")
          multiInput(
            inputId = "family",
            label = "", 
            choices = "",
            selected = "",
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
      ),
      br(),
      
      fluidRow(
        box(
          id = "speciesTable",
          title = div(style = "displayL inline;", icon("images"), "Click on any row in table to view fish images"),
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          background = "primary",
          status = "primary",
          gradient = TRUE,
          div(
            align = "right",
            downloadButton("downloadData", "Download species list", icon = icon("cloud-download-alt"), class = "modButton")
          ),
          #withSpinner(DTOutput("species_table"))
          DTOutput("species_table")
        )
      ),

      #-------------------------------------------------
      # Absolute panel will house the user input widgets
      #-------------------------------------------------
      absolutePanel(top = 100, right = 50, fixed = FALSE,
                    tags$div(style = "opacity: 0.80; background-color: #000000; padding: 8px; color: #FFFFFF; font-size: 1.2em;",
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
