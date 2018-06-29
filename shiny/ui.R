library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "useR2018 Datathon ALA",
      tags$li(class = "dropdown", actionLink("demo_tasmania", span(icon("download"), " Tasmania"))),
      tags$li(class = "dropdown", a(href="https://github.com/mitchelloharawild/emu", target="_blank", span(icon("github"), " GitHub")))
    ),
    dashboardSidebar(
      # sidebarSearchForm("animalSearch")
      uiOutput("ui_year_slider"),
      fluidRow(
        column(8, sliderInput("in_anim_interval",
                              min = 200, max = 1000, step = 100, value = 500,
                              label = "Interval")),
        uiOutput("ui_animBtn")
      ),
      br(),
      uiOutput("ui_genus_filter")
    ),
    dashboardBody(
      # includeScript("https://d3js.org/d3-hexbin.v0.2.min.js"),
      # includeScript("https://d3js.org/d3.v4.min.js"),
      # includeScript("https://rawgit.com/Asymmetrik/leaflet-d3/master/dist/leaflet-d3.js"),
      tags$link(rel="import", href="bower_components/emoji-rain/emoji-rain.html"),
      includeCSS("www/emu.css"),
      div(class="col-sm-8", style = "height:100%; padding-left:0; padding-right:0;",
          leafletOutput("map", width = "100%", height = "100%")
      ),
      div(class="col-sm-4", style = "height:100%; background-color:#23212C;",
          plotOutput("plot_trend"),
          plotOutput("plot_season")
      )
    )
  )
)
