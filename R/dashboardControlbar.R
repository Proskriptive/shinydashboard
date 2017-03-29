
#' Create a dashboard control Right sidebar.
#'
#' TODO: create functions that can create sub-components in control-sidebar,
#' like functions defined in dashboardSidebar.R
#'
#' @param ... Items to put in the Control Bar.
#' @param paneldivs Divs for nav tabs.
#'
#' @export
dashboardControlbar <- function(...) {

  tags$aside(class="control-sidebar control-sidebar-dark",
    div(list(...))
  )

}

#' Create Nav tabs on the Shiny Dashboard Control Sidebar.
#'
#' The tabs can contain tab ID which will be specific to display that tab Content in the div
#' below it
#'
#' @param tabID Shiny Input ID for the Tab anchor link
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.

#' @export
navTabs <- function(tabID, icon = NULL) {
  if (!is.null(icon))
    tagAssert(icon, type = "i")

  tags$li(tags$a(href = paste0("#control-sidebar-", tabID),
    `data-toggle` = "tab", icon))

}


#' Creates the settings Panel Div for the Nav Tabs in the Dashboard Control Bar.
#'
#' It will contain the Tab Panel Items Specific to the General Settings Tab.
#'
#' @param ... Form Items to Put in the Settings Tab Content Panel.
#' @param panelHeading panel heading name
#' @export
settingsTabPanel <- function(..., panelHeading) {
  formPanel <- list(...)
  lapply(formPanel, tagAssert, type = "div")
  if (panelHeading == "General Settings") {
    tags$div(class = "tab-Setting", tags$form(method = "post",
      tags$h3(class = "control-sidebar-heading", panelHeading),
      formPanel))
  } else {
    tags$div(class = "tab-chatSetting", tags$form(method = "post",
      tags$h3(class = "control-sidebar-heading", panelHeading),
      formPanel))
  }

}

#' Creates the Home Panel Div for the Nav Tabs in the Dashboard Control bar.
#'
#' It will contain the Tab Panel Items Specific to the Home Tab.
#'
#' @param  ... List Items to Put in the Home Tab
#' @param panelHeading panel heading name
#'
#' @export
homeTabPanel <- function(..., panelHeading) {
  ListItems <- list(...)
  lapply(ListItems, tagAssert, type = "li")
  tags$div(class = "tab-pane active", id = "control-sidebar-home-tab",
    tags$ul(class = "control-sidebar-menu", tags$h3(class = "control-sidebar-heading",
      panelHeading), ListItems))
}

#' Creates the list items to be embeddded into the Home Tab Panel Content.
#'
#' It will be used to render the items present in the Home tab.
#'
#' @param header This will contain the header text for the list element.
#' @param progressValue This value moves the Progress bar.
#' @param progressBarClass This is used to set the Bootstrap Class for displaying the Progress Bar.
#'
#' @export
listItems <- function(header, progressValue, progressBarClass) {
  if (progressBarClass %in% validStatuses) {
    tags$li(a(href = "javascript:void(0)", tags$h4(class = "control-sidebar-subheading",
      header, tags$span(class = paste0("label label-",
        progressBarClass, " pull-right"), progressValue)),
      tags$div(class = "progress progress-xxs", tags$div(class = paste0("progress-bar progress-bar-",
        progressBarClass), style = paste0("width:", progressValue,
        "%")))))
  }
}



#' Creates the Form for the settings Panel which consists of checkboxes in the dashboard Control bar.
#'
#' It will contain Form to be processed by user
#'
#' @param header This will contain the header text for the form element.
#' @param description This Provides the description for the setting element.
#' @param icon This will append icon to the Settings Element.
#'
#' @export
formPanel <- function(header, description, icon = NULL) {
  if (!is.null(icon)) {
    tagAssert(icon, type = "i")
    tags$div(class = "form-group", tags$label(class = "control-sidebar-subheading",
      header, a(href = "javascript:void(0)", class = "text-red pull-right",
        icon)))

  } else {
    tags$div(class = "form-group", tags$label(class = "control-sidebar-subheading",
      header, customCheckboxInput()), tags$p(description))
  }

}
