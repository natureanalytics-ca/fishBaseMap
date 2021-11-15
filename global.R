
## Load Required Packages## 
library(shiny)
library(fresh)
library(bs4Dash)
library(shinyWidgets)
library(rfishbase)
library(DT)
library(shinyalert)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(rinat)
library(leaflet)
library(leaflet.extras)
library(spData) 
library(waiter)


#------------------------------------------
# Setup theme (fresh library)
# Built on AdminLTE3
# Does not take bootstrap built-in themes
# NULL will retain default value
#------------------------------------------

inputTheme <- fresh::create_theme(
  
  #Custom variables
  bs4dash_vars(
    navbar_light_color = NULL,
    navbar_light_active_color = NULL,
    navbar_light_hover_color = NULL
  ),
  
  #Color contrast
  # bs4dash_yiq(
  #   contrasted_threshold = 10,
  #   text_dark = "#FFF", #allows switch to white if not enough contrast
  #   text_light = "#272c30" #allow swicth to dark grey if not enough contrast
  # ),
  
  #Layout options
  bs4dash_layout(
    font_size_root = NULL,
    sidebar_width = NULL,
    sidebar_padding_x = NULL,
    sidebar_padding_y = NULL,
    sidebar_mini_width = NULL,
    control_sidebar_width = NULL,
    boxed_layout_max_width = NULL,
    screen_header_collapse = NULL,
    main_bg = "#fcfcfc",
    content_padding_x = NULL,
    content_padding_y = NULL
  ),
  
  # "#fcfcfc" a very light grey - main bg
  # "#c2c7d0" light grey - sidebar text
  #"#343a40" Dark grey - side bar bg
  
  #Sidebar skin
  bs4dash_sidebar_light(
    bg = "#343a40",
    hover_bg = "#343a40",
    color = "#c2c7d0",
    hover_color = "#FFFFFF",
    active_color = "#c2c7d0",
    submenu_color = "#c2c7d0",
    submenu_hover_color = "#FFFFFF",
    submenu_active_color = "#FFFFFF",
    submenu_bg = "#343a40",
    submenu_hover_bg = "#343a40",
    submenu_active_bg = "#343a40",
    header_color = "#343a40"
  ),
  
  bs4dash_sidebar_dark(
    bg = "#343a40",
    hover_bg = "#343a40",
    color = "#c2c7d0",
    hover_color = "#FFFFFF",
    active_color = "#c2c7d0",
    submenu_color = "#c2c7d0",
    submenu_hover_color = "#FFFFFF",
    submenu_active_color = "#FFFFFF",
    submenu_bg = "#343a40",
    submenu_hover_bg = "#343a40",
    submenu_active_bg = "#343a40",
    header_color = "#343a40"
  ),
  
  
  #Status custom colors
  bs4dash_status(
    primary = "#78959E",
    secondary = "#A9D1DE",
    success = "#6CCDEB",
    info = "#5098AE",
    warning = NULL,
    danger = "#FA5563",
    light = NULL,
    dark = NULL
  ),
  
  #Main custom colors
  bs4dash_color(
    blue = NULL,
    lightblue = NULL,
    navy = NULL,
    cyan = NULL,
    teal = NULL,
    olive = NULL,
    green = NULL,
    lime = "#343a40",
    orange = NULL,
    yellow = NULL,
    fuchsia = NULL,
    purple = NULL,
    maroon = NULL,
    red = NULL,
    black = NULL,
    gray_x_light = NULL,
    gray_600 = NULL,
    gray_800 = NULL,
    gray_900 = NULL,
    white = "#fcfcfc"
  ),
  
  #Button options
  bs4dash_button(
    default_background_color = NULL,
    default_color = NULL,
    default_border_color = NULL,
    padding_y_xs = NULL,
    padding_x_xs = NULL,
    line_height_xs = NULL,
    font_size_xs = NULL,
    border_radius_xs = NULL
  ),
  
  #Font options
  bs4dash_font(
    size_base = NULL,
    size_lg = NULL,
    size_sm = NULL,
    size_xs = NULL,
    size_xl = NULL,
    weight_light = NULL,
    weight_normal = NULL,
    weight_bold = NULL,
    family_sans_serif = NULL,
    family_monospace = NULL,
    family_base = NULL
  )
)




#---------------
#Fishbase load
#---------------
#Load fishbase country info
Sys.setenv(FISHBASE_HOME=paste0(getwd(), "/www/fb"))
#fish_master<-country()
# fish_master<-fish_master %>%
#   select("country", "Status", "Species")
# saveRDS(fish_master, file = "fishbase_countries.rds")
fish_master<-readRDS( file = "www/fishbase_countries.rds")



#----------------------------------------
#Footer - for NA copyright, terms of use
#----------------------------------------
footer = dashboardFooter(
  left =  HTML(
    "<div style='padding-left: 10px;'>&copy Nature Analytics</div>",
    "<a style='padding-left: 10px;' href='https://natureanalytics.ca/terms' target=
    _blank'>&nbsp &nbsp Terms of use</a>",
    ),
  
  right = 
    tags$a(href="https://natureanalytics.ca", 
           tags$image(src="NA.png", 
                      height=32, 
                      width=32
           ),  
           target="_blank")
)


#-----------------------------
#Recalculating
#----------------------------

#shinycssloaders, attached to re-calc of individual plots, graphics
options(spinner.color="black")

#Waiter, freezes entire window while something loads w/ spinner. i.e. loading a file from server
waitScreen <- Waiter$new(html = tagList(
  spin_loaders(id=19, color = "black")
),
color = transparent(alpha = 0.2),
fadeout = TRUE)
