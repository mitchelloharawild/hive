library(shiny)
library(ggplot2)
library(purrr)
library(leaflet)
library(dplyr)
library(ggimage)

theme_bee <- function() {
  theme(
    axis.title = element_text(colour = "white", face = "bold", size =  18),
    axis.ticks = element_line(colour = "white"),
    axis.text = element_text(colour = "white", size = 14),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "transparent"),
    panel.background = element_rect(fill = "#23212C"),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
}

ele_data <- readr::read_csv("elevation_data.csv")
ele_data_dist <- group_by(ele_data, latitude, longitude) %>%
  summarise(elevation = mean(elevation))
bee <- readr::read_csv("bee_data.csv")
bee_data <- bee %>%
  left_join(ele_data_dist, by = c("latitude", "longitude")) %>%
  filter(elevation > -15)

shinyServer(
  function(input, output, session) {
    pal <- colorFactor("Paired", domain = NULL)
    data <- bee_data %>%
      filter(!(genus%in%c("Ctenoplectra","Nomada", "")),
             !is.na(genus),
             locationQuality) %>%
      mutate(colour = pal(genus))
    year_range <- range(data$year, na.rm = TRUE)


    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 4)) %>%
        addProviderTiles("Esri.WorldTopoMap") %>%
        setView(lng = 133.5, lat = -28, zoom = 5) %>%
        setMaxBounds(101.3379, -9.102097, 172.6611, -44.11914) %>%
        addLegend(pal = pal, values = ~ genus, data = data)
    })

    output$ui_year_slider <- renderUI({
      sliderInput("in_year", "Year",
                  min = year_range[1],
                  max = year_range[2],
                  value = year_range,
                  animate = animationOptions(interval = 500),
                  ticks = FALSE)
    })

    output$ui_genus_filter <- renderUI({
      allgenus <- unique(data$genus)
      selectInput("in_genus", "Genus",
                  choices = allgenus,
                  selected = c("Apis", "Amegilla"),
                  multiple = TRUE)
    })

    output$ui_animBtn <- renderUI({
      if(is.null(input$btn_anim_toggle) || input$btn_anim_toggle %% 2){
        disp <-
          box(
            "Play",
            width = 4,
            background = "green",
            offset = 1
          )
      }
      else{
        disp <-
          box(
            "Stop",
            width = 4,
            background = "red",
            offset = 1
          )
      }
      actionLink(
        "btn_anim_toggle",
        disp
      )
    })


    val_info <- reactiveVal(tagList(
      # p("Bzzz! Welcome to the hive!"),
      # p("We've been bee-zy finding all our honey friends."),
      p("You can zoom and drag the map to explore all the hives."),
      p("Also check out the stats on the right, they automatically update to"))
    )
    output$ui_info <- renderUI({
      div(class = "info_panel",
        box(
          val_info(),
          title = span(icon("info"), "Information"),
          width = 12
        )
      )
    })

    filteredData <- reactive({
      if(is.null(input$in_year)){
        return(NULL)
      }
      out <- data %>%
        filter(genus %in% input$in_genus)

      if(!identical(input$in_year, year_range)){
        out <- out %>%
          filter(
            year >= input$in_year[1],
            year <= input$in_year[2])
      }
      out
    })

    plotData <- reactive({
      if(is.null(input$map_bounds)){
        return(NULL)
      }
      filteredData() %>%
        filter(latitude <= input$map_bounds$north,
               latitude >= input$map_bounds$south,
               longitude <= input$map_bounds$east,
               longitude >= input$map_bounds$west,
               !is.na(year), !is.na(month))
    })

    output$plot_season <- renderPlot({
      if(is.null(plotData())){
        return(NULL)
      }
      plotData() %>%
        count(month) %>%
        ggplot(aes(x=month, y=n)) +
        geom_col(fill = "#FCD615") +
        scale_x_continuous(
          breaks = seq(1, 12, by = 2),
          labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")
        ) +
        xlab("Month") +
        ylab("Occurences") +
        theme_bee()
    }, bg = "transparent")

    output$plot_trend <- renderPlot({
      if(is.null(plotData())){
        return(NULL)
      }
      plotData() %>%
        ggplot(aes(x = genus, y = elevation, fill = colour)) +
        geom_violin(colour = "white") +
        scale_fill_identity() +
        scale_colour_identity() +
        xlab("Genus") +
        ylab("Elevation") +
        coord_flip() +
        theme_bee()
    }, bg = "transparent")

    observe({
      if(is.null(filteredData())){
        return(NULL)
      }
      leafletProxy("map", data = filteredData()) %>%
        clearMarkers() %>%
        addCircleMarkers(lng = ~ longitude, lat = ~ latitude, color = ~ colour,
                         stroke = FALSE, fillOpacity = 0.5, radius = 9)
    })

    observeEvent(input$demo_tasmania, {
      leafletProxy("map") %>%
        flyTo(146.4423, -42.22242, 8)

      val_info(tagList(
        p("You clicked on Tasmania!")
      ))

      updateSelectInput(session, "in_genus", selected = "Bombus")

      anim_range <- c(1977, 1992) - 1
      observe({
        if(anim_range[2] >= year_range[2]){
          return()
        }
        else{
          invalidateLater(millis = 2000)
          new_range <- anim_range
          new_range[2] <- new_range[2] + 3
          anim_range <<- new_range
          updateSliderInput(session, "in_year", value = anim_range)
        }
      })
    })
  }
)
