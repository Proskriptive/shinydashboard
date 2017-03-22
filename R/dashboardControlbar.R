#' Create a dashboard control sidebar.
#'
#' TODO: create functions that can create sub-components in control-sidebar,
#' like functions defined in dashboardSidebar.R
#'
#'@param ... Items to put in the sidebar.
#' @export
dashboardControlbar <- function(ctrlHTML = NULL) {

  if ( is.null(ctrlHTML) ) {

    HTML(paste0(
      '<!-- Control Sidebar -->

      <aside class="control-sidebar control-sidebar-dark">
      <!-- Create the tabs -->
      <ul class="nav nav-tabs nav-justified control-sidebar-tabs">
      <li class="active"><a href="#control-sidebar-home-tab" data-toggle="tab"><i class="fa fa-home"></i></a></li>
      <li><a href="#control-sidebar-settings-tab" data-toggle="tab"><i class="fa fa-gears"></i></a></li>
      <li><a href="#control-sidebar-theme-demo-options-tab" data-toggle="tab"><i class="fa fa-wrench"></i></a></li>
      </ul>
      <!-- Tab panes -->
      <div class="tab-content">
      <!-- Home tab content -->
      <div class="tab-pane active" id="control-sidebar-home-tab">
      <h3 class="control-sidebar-heading">Recent Activity</h3>
      <ul class="control-sidebar-menu">
      <li>
      <a href="javascript::;">
      <i class="menu-icon fa fa-code bg-purple"></i>
      <div class="menu-info">
      <h4 class="control-sidebar-subheading">shinydashboard development</h4>
      <p>TODO: complete control-sidebar module.</p>
      </div>
      </a>
      </li>
      </ul>
      <!-- /.control-sidebar-menu -->
      <h3 class="control-sidebar-heading">Tasks Progress</h3>
      <ul class="control-sidebar-menu">
      <li>
      <a href="javascript::;">
      <h4 class="control-sidebar-subheading">
      Custom Template Design
      <span class="label label-danger pull-right">70%</span>
      </h4>
      <div class="progress progress-xxs">
      <div class="progress-bar progress-bar-danger" style="width: 70%"></div>
      </div>
      </a>
      </li>
      <li>
      <a href="javascript::;">
      <h4 class="control-sidebar-subheading">
      Update Resume
      <span class="label label-danger pull-right">95%</span>
      </h4>
      <div class="progress progress-xxs">
      <div class="progress-bar progress-bar-success" style="width: 95%"></div>
      </div>
      </a>
      </li>
      <li>
      <a href="javascript::;">
      <h4 class="control-sidebar-subheading">
      Backend Framework
      <span class="label label-danger pull-right">68%</span>
      </h4>
      <div class="progress progress-xxs">
      <div class="progress-bar progress-bar-primary" style="width: 68%"></div>
      </div>
      </a>
      </li>
      </ul>
      <!-- /.control-sidebar-menu -->
      </div>
      <!-- /.tab-pane -->
      <!-- Stats tab content -->
      <div class="tab-pane" id="control-sidebar-stats-tab">Stats Tab Content</div>
      <!-- /.tab-pane -->
      <!-- Settings tab content -->
      <div class="tab-pane" id="control-sidebar-settings-tab">
      <form method="post">
      <h3 class="control-sidebar-heading">General Settings</h3>
      <div class="form-group">
      <label class="control-sidebar-subheading">
      Report panel usage
      <input type="checkbox" class="pull-right" checked>
      </label>
      <p>
      Some information about this general settings option
      </p>
      </div>
      <div class="form-group">
            <label class="control-sidebar-subheading">
              Allow mail redirect
              <input type="checkbox" class="pull-right" checked="">
            </label>

            <p>
              Other sets of options are available
            </p>
      </div>
      <!-- /.form-group -->
      <h3 class="control-sidebar-heading">Chat Settings</h3>
      <div class="form-group">
            <label class="control-sidebar-subheading">
      Show me as online
      <input type="checkbox" class="pull-right" checked="">
      </label>
      </div>
      <div class="form-group">
      <label class="control-sidebar-subheading">
      Turn off notifications
      <input type="checkbox" class="pull-right">
      </label>
      </div>
      <div class="form-group">
      <label class="control-sidebar-subheading">
      Delete chat history
      <a href="javascript:void(0)" class="text-red pull-right"><i class="fa fa-trash-o"></i></a>
      </label>
      </div>

      </form>
      </div>
      <!-- /.tab-pane -->
      <div class= "tab-pane" id="control-sidebar-theme-demo-options-tab">
      <div>
      <h4 class = "control-sidebar-subheading">Layout Options</h4>
      <div class = "form-group">
      <label class="control-sidebar-subheading"><input type="checkbox" data-layout="fixed" class="pull-right"> Fixed layout</label>
      <p>Activate the fixed layout. You can not use fixed and boxed layouts together</p>
      </div>
      <div class = "form-group">
      <label class="control-sidebar-subheading"><input type="checkbox" data-layout="sidebar-collapse" class="pull-right"> Toggle Sidebar</label>
      <p>Toggle the left sidebar state (open or collapse)</p>
      </div>
      <div class = "form-group">
      <label class="control-sidebar-subheading"><input type="checkbox" data-layout="sidebar-collapse" class="pull-right"> Toggle Sidebar</label><p>Toggle the left sidebar state (open or collapse)</p>
      </div>
      </div>
      </div>
      </aside>
      <!-- /.control-sidebar -->
      <!-- Add the sidebar"s background. This div must be placed
      immediately after the control sidebar -->
      <div class="control-sidebar-bg"></div>
      '))

  } else {

    ctrlHTML

  }
}

#' Create a dashboard control Right sidebar.
#'
#' TODO: create functions that can create sub-components in control-sidebar,
#' like functions defined in dashboardSidebar.R
#'
#' @param ... Items to put in the Control Bar.
#' @param paneldivs Divs for nav tabs.
#'
#' @export
CreateControlBarRight<-function(..., paneldivs = NULL)
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
CreateNavTabs <- function(tabID ,icon = NULL)
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
CreateSettingsTabPanel <- function(...,panelHeading)
{
  FormPanel <- list(...)
  lapply(FormPanel, tagAssert, type = "div")
  if( panelHeading == "General Settings")
  {
    tags$div(class = "tab-Setting",tags$form(
      method = "post",tags$h3(class = "control-sidebar-heading",panelHeading),
      FormPanel
    ))
  }
  else
  {
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
CreateHomeTabPanel <- function(...,panelHeading)
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
   CreateListItems <- function(Header,ProgressValue,ProgressBarClass)
   {
      if(ProgressBarClass %in% validStatuses)
      {
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
  CreateFormPanel <- function(Header,Description,icon = NULL)
  {
     if(!is.null(icon))
     {
       tagAssert(icon, type = "i")
       tags$div(class = "form-group",tags$label(class = "control-sidebar-subheading", Header,a(href = "javascript:void(0)", class = "text-red pull-right",icon)) )

     }
    else {
      tags$div(class = "form-group",tags$label(class = "control-sidebar-subheading", Header,tags$input(type = "checkbox", class = "pull-right")),
               tags$p(Description))
    }


  }
