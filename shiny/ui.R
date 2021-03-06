library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)

shinyUI(
  dashboardPage(
    title = "Hive: useR2018 Datathon",
    dashboardHeader(
      title = tags$a(href='http://shiny.mitchelloharawild.com/hive', tags$img(src = "logo.png", style = "height:50px;")),
      tags$li(class = "dropdown notification-menu",
              a(href = "#", class = "dropdown-toggle",
                `data-toggle` = "dropdown", icon("globe"), "Demos"),
              tags$ul(class = "dropdown-menu",
                      tags$li(
                        tags$ul(class = "menu",
                                tags$li(actionLink("demo_tasmania","Tasmania")),
                                tags$li(actionLink("demo_brisbane","Brisbane")),
                                tags$li(actionLink("demo_apis","Honey Bees"))
                        )
                      )
              )
      ),
      tags$li(class = "dropdown", a(href="https://www.ala.org.au", target="_blank", span(icon("paw"), "Data from ALA"))),
      tags$li(class = "dropdown", a(href="https://github.com/mitchelloharawild/hive", target="_blank", span(icon("github"), " GitHub"))),
      tags$li(class = "dropdown notification-menu",
              a(href = "#", class = "dropdown-toggle",
                `data-toggle` = "dropdown", icon("users"), "Authors"),
              tags$ul(class = "dropdown-menu",
                      tags$li(
                        tags$ul(class = "menu",
                                tags$li(a(href="http://twitter.com/mitchoharawild", target="_blank", span(icon("twitter"), "@mitchoharawild"))),
                                tags$li(a(href="http://twitter.com/earowang", target="_blank", span(icon("twitter"), "@earowang"))),
                                tags$li(a(href="http://twitter.com/srkobakian", target="_blank", span(icon("twitter"), "@srkobakian")))
                        )
                      )
              )
      )
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
          plotOutput("plot_density", height = "600px"),
          br(),
          plotOutput("plot_season", height = "300px")
      )
    )
  )
)
