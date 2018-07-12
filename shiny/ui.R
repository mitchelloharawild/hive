library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)

shinyUI(
  dashboardPage(
    title = "Hive: useR2018 Datathon",
    dashboardHeader(
      title = tags$a(href='http://shiny.mitchelloharawild.com/hive', tags$img(src = "logo.png", style = "height:50px;")),
      tags$li(class = "dropdown", actionLink("demo_tasmania", span(icon("download"), " Tasmania"))),
      tags$li(class = "dropdown", a(href="https://github.com/mitchelloharawild/emu", target="_blank", span(icon("github"), " GitHub")))
    ),
    dashboardSidebar(
      width = "300px",
      uiOutput("ui_year_slider"),
      uiOutput("ui_genus_filter"),
      hr(),
      uiOutput("ui_info")
    ),
    dashboardBody(
      includeCSS("www/emu.css"),
      div(class="col-sm-8", style = "height:100%; padding-left:0; padding-right:0;",
          leafletOutput("map", width = "100%", height = "100%")
      ),
      div(class="col-sm-4", style = "height:100%; background-color:#23212C;",
          plotOutput("plot_trend", height = "600px"),
          br(),
          plotOutput("plot_season", height = "300px")
      )
    )
  )
)
