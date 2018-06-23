library(shiny)
library(ggplot2)
library(purrr)
library(leaflet)

shinyServer(
  function(input, output, session) {
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 5)) %>%
        addTiles() %>%
        setView(lng = 137, lat = -28, zoom = 5) %>%
        setMaxBounds(101.3379, -9.102097, 172.6611, -44.11914)
    })
  }
)
