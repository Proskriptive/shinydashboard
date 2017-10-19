




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
  fluidPage(
    fluidRow(
      kpi_metric_box(
        id = "upperLeft",
        title = "Popup a Plot (box content)",
        status = "primary",
        width = 6,
        height = 500,
        expandable = TRUE,
        collapsible = FALSE,
        dygraphOutput("example_graph")
      ),
      kpi_metric_box(
        id = "upperRight",
        title = "Popup a Text Document (box content)",
        status = "primary",
        width = 6,
        height = 500,
        expandable = TRUE,
        collapsible = FALSE,
        h2("Modal Popups"),
        span("We need the ability to popup the content of a box within a modal dialog.   The content can be within the ui function, or the server function.  The expand capability that reacts to the the click on the icon in the upper right of the box header
        should pull the content from within the box."),
        br(),
        h3("Features"),
        tags$ul(
          tags$li('Extend Shiny Dashboard Box: box()'),
          tags$li('Button/icon in the upper right',icon("expand")),
          tags$li('Modal Dialog is launched when the expand button is clicked'),
          tags$li('Content in the dialog is cloned from the box body content'),
          tags$li('Reactive inputs work in the cloned modal dialog')
        ),

        br(),
        div(img(src='img/pro_logo.png', style="background-color:#C5ECFC;align=center;",width=200,height=60 ),align="center")
      )
    ),
    fluidRow(
      kpi_metric_box(
        id = "lowerLeft",
        title = "Popup a Data Table (box content)",
        status = "primary",

        width = 12,
        expandable = TRUE,
        collapsible = FALSE,

        DT::dataTableOutput("example_table")
      )


    )
  )
)))

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
    output$example_graph_popup <- renderDygraph({
      dygraph(lungDeaths) %>%
        dySeries("mdeaths", label = "Male") %>%
        dySeries("fdeaths", label = "Female") %>%
        dyOptions(stackedGraph = TRUE) %>%
        dyRangeSelector(height = 20)
    })

    output$example_table <- renderDataTable({
      datatable(iris,options = list(scrollX=TRUE))
    })

    output$example_table_popup <- renderDataTable(datatable(iris,options = list(scrollX=TRUE)))

    observeEvent(input$upperLeft_expand, {
      showModal(ui = modalDialog(
        dygraphOutput("example_graph_popup"),
        title = "Popup",
        easyClose = TRUE,
        fade = TRUE,
        size = "l"
      ))

    })


    observeEvent(input$lowerLeft_expand, {
      showModal(ui = modalDialog(
        dataTableOutput("example_table_popup"),
        title = "Popup",
        easyClose = TRUE,
        fade = TRUE,
        size = "l"
      ))

    })
    observeEvent(input$upperRight_expand, {
      showModal(ui = modalDialog(
        h2("Modal Popups"),
        span("We need the ability to popup the content of a box within a modal dialog.   The content can be within the ui function, or the server function.  The expand capability that reacts to the the click on the icon in the upper right of the box header
        should pull the content from within the box."),
        br(),
        h3("Features"),
        tags$ul(
          tags$li('Extend Shiny Dashboard Box: box()'),
          tags$li('Button/icon in the upper right',icon("expand")),
          tags$li('Modal Dialog is launched when the expand button is clicked'),
          tags$li('Content in the dialog is cloned from the box body content'),
          tags$li('Reactive inputs work in the cloned modal dialog')
        ),

        br(),
        div(img(src='img/pro_logo.png', style="background-color:#C5ECFC;align=center;",width=200,height=60 ),align="center"),    title = "Popup",
        easyClose = TRUE,
        fade = TRUE,
        size = "l"
      ))

    })
  }
)
