library(shiny)
library(shinydashboard)
library(leaflet)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "useR2018 Datathon ALA",
      tags$li(class = "dropdown", actionLink("btn_template", span(icon("download"), " Template Button"))),
      tags$li(class = "dropdown", a(href="https://github.com/mitchelloharawild/emu", target="_blank", span(icon("github"), " GitHub")))
    ),
    dashboardSidebar(
      # sidebarSearchForm("animalSearch")
      uiOutput("ui_year_slider"),
      uiOutput("ui_genus_filter")
    ),
    dashboardBody(
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
