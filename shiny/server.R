library(shiny)
library(ggplot2)
library(purrr)
library(leaflet)
library(dplyr)

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


    anim_range <- reactiveVal(NULL)
    anim_trigger <- reactiveVal(FALSE)
    demo_anim <- function(start_time, end_time = year_range[2], delay, step, stretch = FALSE){
      anim_range(start_time[[1]] - 1)
      anim_trigger(!anim_trigger())
    }

    observe({
      anim_trigger()
      isolate(.anim_range <- anim_range())
      if(is.null(.anim_range)){
        return()
      }
      if(.anim_range[2] >= max(demo_par()$times[[demo_pos()]])[[1]]){
        return()
      }
      else{
        invalidateLater(millis = demo_par()$delay[[demo_pos()-1]])
        new_range <- .anim_range
        if(demo_par()$stretch[[demo_pos()-1]]){
          new_range[2] <- new_range[2] + demo_par()$step[[demo_pos()-1]]
        }
        else{
          new_range <- new_range + demo_par()$step[[demo_pos()-1]]
        }
        anim_range(new_range)
        updateSliderInput(session, "in_year", value = new_range)
      }
    })


    vis_btn_next <- reactiveVal(FALSE)
    demo_par <- reactiveVal(list())
    demo_pos <- reactiveVal(NULL)

    observeEvent(input$btn_demo_next, {
      demo_pos(demo_pos() + 1)
    })

    observeEvent(demo_pos(), {
      req(demo_pos())
      i <- demo_pos()
      if(i > 1){
        demo_anim(demo_par()$times[i-1], max(demo_par()$times[[i]]),
                  demo_par()$delay[[i-1]], demo_par()$step[[i-1]],
                  demo_par()$stretch[[i-1]])
      }
      if(i == length(demo_par()$times)){
        vis_btn_next(FALSE)
        return()
      }
      val_info(demo_par()$info[[i]])
      leafletProxy("map") %>%
        flyTo(demo_par()$position[[i]][1], demo_par()$position[[i]][2], demo_par()$zoom[[i]])
      updateSliderInput(session, "in_year", value = demo_par()$times[[i]])
      updateSelectInput(session, "in_genus", selected = demo_par()$genus[[i]])
    })

    demo_build <- function(times, info, genus, position, zoom, delay, step, stretch = FALSE){
      demo_par(as.list(environment()))
      i <- 0
      vis_btn_next(TRUE)
      demo_pos(1L)
    }

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
                  animate = animationOptions(interval = 500,
                                             playButton = span("Start animation", icon("play")),
                                             pauseButton = span("Stop animation", icon("pause"))
                                             ),
                  ticks = FALSE)
    })

    output$ui_genus_filter <- renderUI({
      allgenus <- unique(data$genus)
      selectInput("in_genus", "Genus",
                  choices = allgenus,
                  selected = c("Apis", "Amegilla"),
                  multiple = TRUE)
    })

    val_info <- reactiveVal(tagList(
      p("Bzzz! Welcome to the Hive"),
      p("We've been bee-zy finding all our bee-autiful friends in Australia."),
      p("You can zoom and drag the map to explore all the hives.
        Each genus has a special colour,
        the dots of this colour on the map show where our ALA explorers have spotted us."),
      p("If you really want to fly, and know where our bees will be in the sky, check out the elevation plot.
        This tells you the sea heights where the bees displayed on the map were spotted.
        It will automatically update when the genus or time range change."),
      p("You can also see the time of year when the bees were spotted,
        remember it may bee all the ALA explorers staying inside that makes bees tricky to spot.")
      )
    )
    output$ui_info <- renderUI({
      div(class = "info_panel",
        box(
          val_info(),
          title = span(icon("info"), "Information"),
          width = 12
        ),
        div(style = "text-align:center; height:10px; line-height:10px;",
            actionLink(
              "btn_demo_next",
              uiOutput("ui_demo_ops")
          )
        )
      )
    })

    output$ui_demo_ops <- renderUI({
      if(vis_btn_next()){
        box(p("Next", style = "color:#333333"), width = 4, height = "34px")
      }
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
      out %>%
        sample_n(min(NROW(out), 1000))
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
      if(is.null(plotData()) || NROW(plotData()) == 0) {
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

    output$plot_density <- renderPlot({
      if(is.null(plotData()) || NROW(plotData()) == 0) {
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
        demo_build(times = list(c(1977, 1990), c(1990,  year_range[2]), year_range[2]),
                   info = list(tagList(p("You clicked on Tasmania!"),
                                       p("Located south of Australia's east coast,
                                         Tasmania is separated from the mainland by over 200km.
                                         We are going to take a look at the genus Bombus,
                                         these are bumble bees,
                                         and they didn't land in Tasmania until 1992."),
                                       p("Hit Next to take a peek at their arrival.")),
                               tagList(p("The first Bumble bee landed in Hobart. As the dots show, he wasn't alone for long.
                                         Bumble bees adventured west into the national park.
                                         They then flew north and have continued to spread across the island."),
                                       p("Our little foreign friends finally arrived on the mainland in 2018. Can you spot them?"))),
                   genus = c("Bombus", "Bombus"),
                   position = list(c(146.44775390625, -28.01),c(146.233520507813, -41.0710691308064)),
                   zoom=c(5, 7),
                   delay=c(2000, 2000),
                   step = c(3, 1),
                   stretch = c(TRUE, TRUE))
    })

    observeEvent(input$demo_apis, {
        demo_build(times = list(c(year_range[1], 1940), c(1977, 1992) + 6, year_range[2]),
                   info = list(tagList(p("Many people in Australia like to live close together in large cities.
                                         So do the Apis genus of bees."),
                                       p("Known as the European honey bee,
                                         they have been found in Australia since the early 1800s.
                                         There are more recorded Apis observations than the other genus of bees, this means they were spotted more.")),
                               tagList(p("Their numbers in Australia have exploded in the past 80 years, this is likely because they are often found in managed hives."),
                                       p("You can see that people often located these bees near populations, could this bee due to them belonging to people's hives and homes?")
                                       )
                               ),
                   genus = c("Apis", "Apis"),
                   position = list(c(132.930616736412, -28.01),c(146.4423, -42.22242)),
                   zoom=c(5, 5),
                   delay=c(2000, 2000),
                   step = c(3, 1),
                   stretch = c(TRUE, TRUE))
    })

    observeEvent(input$demo_brisbane, {
        demo_build(times = list(c(year_range[1], year_range[2]), c(year_range[1], year_range[2]), year_range[2]),
                   info = list(tagList(p("Welcome to Brisbane!"),
                                       p("The third largest city in Australia is home to not only a diverse community of people,
                                         but also a diverse community of bees."),
                                       p("The bars show us that not many of the Exoneura and Exoneurella genus of bees are found in Brisbane city.
                                         They were spotted around and in national parks.")),
                               tagList(p("We can take a look at where any type of bee was spotted in Brisbane."),
                                       p("Each coloured dot represents a different bee found on the map.
                                         You can find out the genus represented by each colour by looking at the list at the top of the map."),
                                       p("Which genus has been spotted the least?")
                                  )),
                   genus = list(c("Exoneura", "Exoneurella"),
                                c("Exoneura", "Exoneurella",
                                  "Apis", "Amegilla", "Braunsapis",
                                  "Bombus", "Tetragonula", "Thyreus",
                                  "Ceratina", "Xylocopa", "Austroplebeia")),
                   position = list(c(152.4527, -27.206),c(153.02145, -27.47438)),
                   zoom=c(9, 12),
                   delay=c(2000, 2000),
                   step = c(3, 1),
                   stretch = c(TRUE, TRUE))
    })
  }
)
