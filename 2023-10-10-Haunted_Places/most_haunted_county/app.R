# library(shiny)
library(leaflet)
library(sf)
library(tidyr)
library(dplyr)

# Data ------------------------------------------------------------------

haunted_places <- read_sf("output_data/haunted_places_county-final.shp")
haunted_places$lng <- purrr::map_vec(haunted_places$geometry, ~st_coordinates(.))[,1] %>% as.character()
haunted_places$lat <- purrr::map_vec(haunted_places$geometry, ~st_coordinates(.))[,2] %>% as.character()
place_desc <- read.csv("output_data/place_desc.csv")
haunted_places <-
  haunted_places %>%
  select(!dscrptn) %>%
  left_join(place_desc, by="id")
state_filter <- read_sf("output_data/state_filter.shp")
place_desc <- read.csv("output_data/place_desc.csv")
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

## ghost icon https://icons8.com/icons/set/ghost
ghostIcons <- icons(
  iconUrl = './icon/icons8-ghost-48.png',
  iconWidth = 23, iconHeight = 23
)

# Dashboard ----------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("www/style.css"),
  h1("Haunted Places in Los Angeles County"),
  h2(HTML('LA County is the most haunted county in the United States, with over 300 reported haunted places. Click the ghosts to learn more about each haunted place.')),
  fluidRow(
    column(4,
           br(), br(),
           h3(textOutput("location")),
           h4(textOutput("city")),
           br(),
           textOutput("description"),
           uiOutput("image")
    ),
    column(8,
           leafletOutput("map", height=700),
           actionButton("reset_button", "Reset view", class="button"),
           br(),
           HTML("<p>Data: Snopes Horrors Section<br>Made by <a href='https://github.com/kkakey'>@kkakey</a></p>")
    )
  )

)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(-118.9, 33.68, -117.7, 34.8) %>%
      setView(lat = 34.2, lng = -118.11, zoom = 9) %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons(data = state_filter, stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
                  color = "black", opacity = 1) %>%
      addMarkers(data = haunted_places, icon = ghostIcons,
                  popup = ~locatin)
  })

  observe({input$reset_button
    leafletProxy("map") %>% setView(lat = 34.2, lng = -118.11, zoom = 9)
  })

  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    loc <- haunted_places[which(haunted_places$lat == as.character(click$lat) & haunted_places$lng == as.character(click$lng)), ]$locatin[1]
    city <- haunted_places[which(haunted_places$lat == as.character(click$lat) & haunted_places$lng == as.character(click$lng)), ]$city[1]
    desc <- haunted_places[which(haunted_places$lat == as.character(click$lat) & haunted_places$lng == as.character(click$lng)), ]$description[1]
    img_url_click <- haunted_places[which(haunted_places$lat == as.character(click$lat) & haunted_places$lng == as.character(click$lng)), ]$img_url[1]
    # https://www.pinterest.com/pin/characters--579979258278605737/
    img_url_click <- ifelse(is.na(img_url_click), 'Halloween Play Sticker by Andrew Onorato for iOS & Android _ GIPHY.gif', img_url_click)

    output$location <- renderText({
      stringr::str_to_title(loc)
    })

    output$city <- renderText({
      paste(" in ", stringr::str_to_title(city))
    })

    output$description <- renderText({
      firstup(desc)
    })

    output$image <- renderUI({
      tags$img(src = img_url_click, height="100%", width="100%", align="left")
    })

  })
}

shinyApp(ui = ui, server = server)
