library(shiny)
library(leaflet)
library(sf)

# Data ------------------------------------------------------------------

haunted_places <- read_sf("../output_data/haunted_places_county.shp")
state_filter <-read_sf("../output_data/state_filter.shp")

## ghost icon https://icons8.com/icons/set/ghost
ghostIcons <- icons(
  iconUrl = '/Users/kristenakey/Downloads/icons8-ghost-48-color.png',
  iconWidth = 23, iconHeight = 23#,
  # shadowUrl = '/Users/kristenakey/Downloads/output-onlinepngtools.png',
  # shadowWidth = 26, shadowHeight = 26,
  # shadowAnchorX = .01, shadowAnchorY = .01
)

# Dashboard ----------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

  fluidRow(
    column(4,
           h1("Header here"),
           h3("more text"),
           plotOutput("imageOutput")
    ),
    column(8,
           leafletOutput("mymap", height=700)
    )
  )
  # actionButton("recalc", "New points")

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)

  output$mymap <- renderLeaflet({
    leaflet() %>%
      fitBounds(-118.9, 33.68, -117.7, 34.8) %>%
      # setView(lat = 34.2, lng = -118.11, zoom = 9) %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons(data = state_filter, stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
                  color = "black", opacity = 1) %>%
      addMarkers(data = haunted_places, icon = ghostIcons,
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)


## to do:
### add auto zoom in on county
### on-hover: place info show ups
### on-click: photo shows up
## ghost shadows?
