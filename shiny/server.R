library(shiny)
library(ggplot2)
library(purrr)
library(leaflet)
library(dplyr)

shinyServer(
  function(input, output, session) {
    data <- readr::read_csv("bee_data.csv") %>%
      filter(!(genus%in%c("Ctenoplectra","Nomada", "")),
             !is.na(genus))

    pal <- colorFactor("Dark2", domain = NULL)

    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 5)) %>%
        addProviderTiles("Esri.WorldTopoMap") %>%
        setView(lng = 137, lat = -28, zoom = 5) %>%
        setMaxBounds(101.3379, -9.102097, 172.6611, -44.11914)
    })

    output$ui_year_slider <- renderUI({
      year_range <- range(data$year, na.rm = TRUE)
      sliderInput("in_year", "Year slider",
                  min = year_range[1],
                  max = year_range[2],
                  value = year_range,
                  animate = animationOptions(interval = 500),
                  ticks = FALSE)
    })

    output$ui_genus_filter <- renderUI({
      allgenus <- unique(data$genus)
      selectInput("in_genus", "Filter genus",
                  choices = allgenus,
                  selected = allgenus,
                  multiple = TRUE)
    })

    filteredData <- reactive({
      if(is.null(input$in_year)){
        return(data)
      }
      data %>%
        filter(genus %in% input$in_genus,
               year >= input$in_year[1],
               year <= input$in_year[2])
    })

    observe({
      leafletProxy("map", data = filteredData()) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(lng = ~ longitude, lat = ~ latitude, color = ~ pal(genus),
                         stroke = FALSE, fillOpacity = 0.5) %>%
        addLegend(pal = pal, values = ~ genus)
    })
  }
)
