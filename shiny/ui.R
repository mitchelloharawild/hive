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
    ),
    dashboardBody(
      includeCSS("www/emu.css"),
      leafletOutput("map", width = "100%", height = "100%")
    )
  )
)
