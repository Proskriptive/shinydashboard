
#' Create a dashboard control Right sidebar.
#'
#' TODO: create functions that can create sub-components in control-sidebar,
#' like functions defined in dashboardSidebar.R
#'
#' @param ... Items to put in the Control Bar.
#' @param paneldivs Divs for nav tabs.
#'
#' @export
dashboardControlbar<-function(..., paneldivs = NULL)
{
  items <- list(...)

  Tabdiv <- list(paneldivs)
  lapply(Tabdiv, tagAssert,type = "div")
  lapply(items, tagAssert, type = "li")
  tags$div(class = "Control-Bar Area",
           tags$aside(class = "control-sidebar control-sidebar-dark",tags$ul(class = "nav nav-tabs nav-justified control-sidebar-tabs",items)
                      ,Tabdiv),
           tags$div(class = "control-sidebar-bg")
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
createNavTabs <- function(tabID ,icon = NULL)
{
  if(!is.null(icon)) tagAssert(icon,type = "i")

     tags$li(tags$a(href = paste0("#control-sidebar-",tabID),`data-toggle` = "tab",icon)
     )

}


#' Creates the settings Panel Div for the Nav Tabs in the Dashboard Control Bar.
#'
#' It will contain the Tab Panel Items Specific to the General Settings Tab.
#'
#' @param ... Form Items to Put in the Settings Tab Content Panel.
#' @param panelHeading panel heading name
#' @export
createSettingsTabPanel <- function(...,panelHeading)
{
  FormPanel <- list(...)
  lapply(FormPanel, tagAssert, type = "div")
  if( panelHeading == "General Settings")  {
    tags$div(class = "tab-Setting",tags$form(
      method = "post",tags$h3(class = "control-sidebar-heading",panelHeading),
      FormPanel
    ))
  }  else  {
    tags$div(class = "tab-chatSetting",tags$form(
      method = "post",tags$h3(class = "control-sidebar-heading",panelHeading),
      FormPanel
    ))
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
createHomeTabPanel <- function(...,panelHeading)
{
  ListItems <- list(...)
  lapply(ListItems, tagAssert, type = "li")
  tags$div(class = "tab-pane active", id = "control-sidebar-home-tab",tags$ul(class = "control-sidebar-menu",tags$h3(class = "control-sidebar-heading",panelHeading),ListItems))
}

#' Creates the list items to be embeddded into the Home Tab Panel Content.
#'
#' It will be used to render the items present in the Home tab.
#'
#' @param Header This will contain the header text for the list element.
#' @param ProgressValue This value moves the Progress bar.
#' @param ProgressBarClass This is used to set the Bootstrap Class for displaying the Progress Bar.
#'
#' @export
createListItems <- function(Header,ProgressValue,ProgressBarClass)
   {
      if(ProgressBarClass %in% validStatuses)   {
        tags$li(a(href = "javascript:void(0)",tags$h4(class = "control-sidebar-subheading",Header,tags$span(class = paste0("label label-",ProgressBarClass," pull-right"),ProgressValue)),
                  tags$div(class = "progress progress-xxs",tags$div(class = paste0("progress-bar progress-bar-",ProgressBarClass),style = paste0("width:",ProgressValue,"%")))))
      }
   }



#' Creates the Form for the settings Panel which consists of checkboxes in the dashboard Control bar.
#'
#' It will contain Form to be processed by user
#'
#' @param Header This will contain the header text for the form element.
#' @param Description This Provides the description for the setting element.
#' @param icon This will append icon to the Settings Element.
#'
#' @export
createFormPanel <- function(Header,Description,icon = NULL)
  {
     if(!is.null(icon))   {
       tagAssert(icon, type = "i")
       tags$div(class = "form-group",tags$label(class = "control-sidebar-subheading", Header,a(href = "javascript:void(0)", class = "text-red pull-right",icon)) )

     }  else {
      tags$div(class = "form-group",tags$label(class = "control-sidebar-subheading", Header,customCheckboxInput()),
               tags$p(Description))
    }

  }
