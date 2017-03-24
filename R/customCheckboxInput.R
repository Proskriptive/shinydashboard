#' Create a Checkbox Custom COntrol for shinyDashboard.
#'
#' It will write a default chekcbox input without any surrounding Div
#'
#' @param inputId It will act as a identifier to the checkbox control element
#' @param label This wil Place the text along with the checkbox control
#' @param value it will specify true or false whether i.e checked or not
#'
#' @export
customCheckboxInput <- function(inputId = NULL,label = NULL,value = FALSE) {
  tags$input(type = "checkbox", class = "pull-right")
}
