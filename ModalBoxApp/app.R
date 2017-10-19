

#
#devtools::install_github("Proskriptive/shinydashboard",ref="Param-dev")
library(shiny)
library(shinydashboard)
library(dygraphs)
library(DT)
library(dplyr)

lungDeaths <- cbind(mdeaths, fdeaths)
header <- dashboardHeader(user = NULL)

sidebar <-
  dashboardSidebar(sidebarMenu(# Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    )))

body <- dashboardBody(tabItems(tabItem(
  "dashboard",
  fluidPage(fluidRow(
    kpi_metric_box(
      id = "upperLeft",
      title = "Popup a Plot (box content)",
      status = "primary",
      width = 6,
      expandable = TRUE,
      collapsible = FALSE,
      dygraphOutput("example_graph")
    ),
    kpi_metric_box(
      id = "upperRight",
      title = "Popup a Text Document (box content)",
      status = "primary",
      width = 6,
      expandable = TRUE,
      collapsible = FALSE,
      fluidRow("Text Data"),footer = "Upper Right"
    )),
    fluidRow(
      kpi_metric_box(
        id = "lowerLeft",
        title = "Popup a Data Table (box content)",
        status = "primary",
        width = 6,
        expandable = TRUE,
        collapsible = FALSE,
        
        DT::dataTableOutput("example_table")
      )
      ,
      kpi_metric_box(
        id = "lowerRight",
        title = "Popup a modal form (reactive)",
        status = "primary",
        width = 6,
        expandable = TRUE,
        collapsible = FALSE,
        div(h1("I want this to popup a different form"))
      )
    ))
  )
))

shinyApp(
  ui = dashboardPage(skin = "blue-light", header, sidebar, body),
  server = function(input, output) {
    output$example_graph <- renderDygraph({
      dygraph(lungDeaths) %>%
        dySeries("mdeaths", label = "Male") %>%
        dySeries("fdeaths", label = "Female") %>%
        dyOptions(stackedGraph = TRUE) %>%
        dyRangeSelector(height = 20)
    })
    
    output$example_table <- renderDataTable({
      datatable(iris[1:4, ])
    })
  }
)
