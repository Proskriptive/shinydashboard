## This tries to render a dashboard with many different components, incl. sidebar, dropdown menus etc.

library(shiny)
library(shinydashboard)
library(dygraphs)
library(xts)
library(Widgets)
library(metricsgraphics)
library(highcharter)
library(magrittr)

header <- dashboardHeader(
  title = "Dashboard Demo",

  # Dropdown menu for messages
  dropdownMenu(
    type = "messages",
    badgeStatus = "success",
    messageItem("Support Team",
                "This is the content of a message.",
                time = "5 mins"),
    messageItem("Support Team",
                "This is the content of another message.",
                time = "2 hours"),
    messageItem("New User",
                "Can I get some help?",
                time = "Today")
  ),

  # Dropdown menu for notifications
  dropdownMenu(
    type = "notifications",
    badgeStatus = "warning",
    notificationItem(
      icon = icon("users"),
      status = "info",
      "5 new members joined today"
    ),
    notificationItem(
      icon = icon("warning"),
      status = "danger",
      "Resource usage near limit."
    ),
    notificationItem(
      icon = icon("shopping-cart", lib = "glyphicon"),
      status = "success",
      "25 sales made"
    ),
    notificationItem(
      icon = icon("user", lib = "glyphicon"),
      status = "danger",
      "You changed your username"
    )
  ),

  # Dropdown menu for tasks, with progress bar
  dropdownMenu(
    type = "tasks",
    badgeStatus = "danger",
    taskItem(value = 20, color = "aqua",
             "Refactor code"),
    taskItem(value = 40, color = "green",
             "Design new layout"),
    taskItem(value = 60, color = "yellow",
             "Another task"),
    taskItem(value = 80, color = "red",
             "Write documentation")
  )
)
sidebar <- dashboardSidebar(
  sidebarUserPanel(
    "User Name",
    subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
    # Image file should be in www/ subdir
    image =paste0("shinydashboard", "-", as.character(utils::packageVersion("shinydashboard")), "/img/guang.jpg")
  ),
  sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem(
      "Widgets",
      icon = icon("th"),
      tabName = "widgets",
      badgeLabel = "new",
      badgeColor = "green"
    ),
    menuItem(
      "Charts",
      icon = icon("bar-chart-o"),
      menuSubItem("Dygraph Plot", tabName = "dygraph"),
      menuSubItem("Multi-line Chart", tabName = "multilinechart")
    ),
    menuItem(
     "Tables",
     icon = icon("table"),
     tabName = "Tables"
    )
  ),
  sidebarMenuOutput("menu"),
  sidebarFooter("Test Footer",img(src="img/Proskriptive-logo.png",width=100),color = "black")

)

controlbar <- dashboardControlbar(

  navTabs(
    tabID = "home-tab",
    icon = icon("home")),
  navTabs(
    tabID = "settings-tab",
    icon = icon("gears")),
  paneldivs =  {
    div( class = "tab-content" ,
    div(class = "tab-pane",id = "control-sidebar-settings-tab",
     settingsTabPanel(
      formPanel("Report Panel Usage","some Information about this general setting option"),
      formPanel("Allow mail redirect","Other sets of options are available"),
      formPanel("Expose author name in posts","Allow the user to show his name in blog posts"),
        panelHeading =  "General Settings"
                           ),
    settingsTabPanel(
      formPanel("Show me as Online",""),
      formPanel("Turn off Notifications",""),
      formPanel("Delete Chat History","",icon = icon("trash")),
          panelHeading =  "Chat settings")),
    homeTabPanel(
      listItems(header = "Custom Template Design",
                      progressValue = 70,
                      progressBarClass = "danger")
     ,listItems(header = "Update Resume",
                      progressValue = 95,
                      progressBarClass = "success")
     ,listItems(header = "Laravel Integration",
                      progressValue = 50,
                      progressBarClass = "warning")
                     ,panelHeading = "Tasks Progress")
                                     )
                                     }
)

datum <-  data.frame(Date = c("12-12-2012", "13-12-2012", "14-12-2012", "15-12-2012", "16-12-2012",
                              "17-12-2012"), MktCap = c(110, 120, 130, 150 ,200, 180), PE = c(18, 18, 17, 18.5,
                                                                                         19, 19))
date <-  as.Date(as.character(datum$Date), "%d-%m-%Y")
mktCap <- as.numeric(datum$MktCap)
mcData <- xts(mktCap, order.by = date)

plotdata <- function() {
  winners <- data.frame(year=1966:1971, mensec=c(8231, 8145, 8537, 8029, 7830, 8325), womensec=c(12100, 12437, 12600, 12166, 11107, 11310))
  men <- ts(winners$mensec, frequency = 1, start=winners$year[1])
  women <- ts(winners$womensec, frequency = 1, start=winners$year[1])
  both <- cbind(men=as.xts(men), women=as.xts(women))
  return(both)
}
hseriesData <- data.frame( months = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                           london = c(4.5,3.4,6.7,7.8,4.4,12.3,9.7,12.5,6.6,9.2,11.1,4.8),
                           berlin = c(-0.9,0.6,3.5,8.4,13.5,17.0,18.6,17.9,14.3,9.0,3.9,1.0))
chartData_multiline <- data.frame(dates = as.Date(1:7, origin=Sys.Date()),
                                  A = c(5,4,6,3,2,7,1),
                                  B = c(7,1,4,6,0,2,3),
                                  C = c(2,0,3,9,5,7,4), stringsAsFactors = FALSE)



body <- dashboardBody(tabItems(
  tabItem("dashboard",
          div(p(
            "Dashboard tab content"
          ))),
  tabItem("widgets",
          "Widgets tab content"),
  tabItem("dygraph",
          box(
            controlChartDygraph(tsData = plotdata(), headerText = "Marathon timings Men and Women",
                                enableDyrange = TRUE, enableDyhighlight = TRUE), width = 8, title = "ChartsJS > Dygraph"
          )),
  tabItem("multilinechart",
          box(
            multilineChart(timeSlots = chartData_multiline$dates, multilinePlotingdata = chartData_multiline),
            width = 12,
            status = "primary",
            title = "ChartJS > Multi-Line Chart"
          )
          ),
  tabItem("Tables",
          box(
            title = " Sample Table",width = NULL,status = "primary",
            div(style = 'overflow-x:scroll',tableOutput('table'))
          ))),

  # Boxes need to be put in a row (or column)
  fluidRow(box(plotOutput("plot1", height = 250)),
           box(
             title = "Controls",
             sliderInput("slider", "Number of observations:", 1, 100, 50)
           )),

  # infoBoxes
  fluidRow(
    infoBox(
      "Orders",
      uiOutput("orderNum2"),
      "Subtitle",
      icon = icon("credit-card")
    ),
    infoBox(
      "Approval Rating",
      "60%",
      icon = icon("line-chart"),
      color = "green",
      fill = TRUE
    ),
    infoBox(
      "Progress",
      uiOutput("progress2"),
      icon = icon("users"),
      color = "purple"
    )
  ),

  # valueBoxes
  fluidRow(
    valueBox(
      uiOutput("orderNum"),
      "New Orders",
      icon = icon("credit-card"),
      href = "http://google.com"
    ),
    valueBox(
      tagList("60", tags$sup(style = "font-size: 20px", "%")),
      "Approval Rating",
      icon = icon("line-chart"),
      color = "green"
    ),
    valueBox(
      htmlOutput("progress"),
      "Progress",
      icon = icon("users"),
      color = "purple"
    )
  ),

  # Boxes
  fluidRow(
    box(
      status = "primary",
      sliderInput(
        "orders",
        "Orders",
        min = 1,
        max = 2000,
        value = 650
      ),
      selectInput(
        "progress",
        "Progress",
        choices = c(
          "0%" = 0,
          "20%" = 20,
          "40%" = 40,
          "60%" = 60,
          "80%" = 80,
          "100%" = 100
        )
      )
    ),
    box(
      title = "Histogram box title",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      plotOutput("plot", height = 250)
    )
  ),

  # Boxes with solid color, using `background`
  fluidRow(
    # Box with textOutput
    box(
      title = "Status summary",
      background = "green",
      width = 4,
      textOutput("status")
    ),

    # Box with HTML output, when finer control over appearance is needed
    box(
      title = "Status summary 2",
      width = 4,
      background = "red",
      uiOutput("status2")
    ),

    box(
      width = 4,
      background = "light-blue",
      p("This is content. The background color is set to light-blue")
    )
  ),

  fluidRow(
    tabBox(
      title = "First tabBox",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("Tab1", "First tab content"),
      tabPanel("Tab2", "Tab content 2")
    ),
    tabBox(
      side = "right", height = "250px",
      selected = "Tab3",
      tabPanel("Tab1", "Tab content 1"),
      tabPanel("Tab2", "Tab content 2"),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
    )
  ),
  fluidRow(
    tabBox(
      # Title can include an icon
      title = tagList(shiny::icon("gear"), "tabBox status"),
      tabPanel("Tab1",
               "Currently selected tab from first box:",
               verbatimTextOutput("tabset1Selected")
      ),
      tabPanel("Tab2", "Tab content 2")
    )
  ),
  fluidRow(
    box(kpi_table_box("kpi_table_box"))

    ,box(kpi_metric_box("kpi_metric_box"))
  ),
  fluidRow(
    box(highChart(title = "Temperature for some cities", theme = "economist", seriesData = hseriesData, seriesCategories = hseriesData$months))
  )

)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)


  output$menu <- renderMenu({
    sidebarMenu(menuItem("Menu item", icon = icon("calendar")))
  })

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  predicted <- reactive({
    hw <- HoltWinters(ldeaths)
    predict(hw, n.ahead = input$months,
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  output$point <- renderText({
    paste0('X = ', strftime(req(input$dygraph_click$x_closest_point), "%d %b %Y"),
           '; Y = ', req(input$dygraph_click$y_closest_point))
  })


  output$plot2 <- renderDygraph({
    lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
    dyRangeSelector(dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)"), dateWindow = c("1974-01-01", "1980-01-01"))
  })

  output$orderNum <- renderText({
    prettyNum(input$orders, big.mark = ",")
  })

  output$orderNum2 <- renderText({
    prettyNum(input$orders, big.mark = ",")
  })

  output$progress <- renderUI({
    tagList(input$progress, tags$sup(style = "font-size: 20px", "%"))
  })

  output$progress2 <- renderUI({
    paste0(input$progress, "%")
  })

  output$status <- renderText({
    paste0(
      "There are ",
      input$orders,
      " orders, and so the current progress is ",
      input$progress,
      "%."
    )
  })

  output$status2 <- renderUI({
    iconName <- switch(input$progress,
                       "100" = "ok",
                       "0" = "remove",
                       "road")
    p("Current status is: ", icon(iconName, lib = "glyphicon"))
  })


  output$plot <- renderPlot({
    hist(rnorm(input$orders))
  })

  # The currently selected tab from the first box
  output$tabset1Selected <- renderText({
    input$tabset1
  })


  output$table <- renderTable({
    test.table
  })
}

ui <- dashboardPage(header,
                    sidebar,
                    body, controlbar)



shinyApp(ui, server)
