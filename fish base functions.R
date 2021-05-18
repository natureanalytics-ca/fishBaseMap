

library(shinyWidgets)
library(rfishbase)
library(dplyr)


#Output from leaflet
countryIn<-"Aruba"


X<-country()

Y<-X %>%
    filter(
      country %in% countryIn,
      Status %in% c("endemic", "native", "introduced", "reintroduced")
    )



Y$Species