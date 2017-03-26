#' The footer of a dashboard page.
#'
#' The footer typically contains copyright info. Another common use pattern
#' is for the footer to contain dynamic message commnuncation.
#'
#' @param ... Items to put in the dashboard main footer
#' @param mainText A character or html argument.  Defines the left hand part of the footer.
#' @param subText A character or html argument.  Defines the right hand part of the footer.

#'
#' @seealso \code{\link{tabItems}}, \code{\link{box}}, \code{\link{valueBox}}.
#'
#' @export
dashboardFooter <- function(
  mainText = "",
  subText  = NULL) {

  tags$footer(
    class="main-footer",
    if (!missing(subText)) {
      tags$div(
        class="pull-right hidden-xs",
        subText
      )
    }else{},
    mainText
  )

}


#' Create a dynamic footer output for shinydashboard (client side)
#'
#' This can be used as a placeholder for dynamically-generated \code{\link{dashboardFooter}}.
#'
#' @param outputId Output variable name.
#' @param tag A tag function, like \code{tags$li} or \code{tags$ul}.
#'
#' @seealso \code{\link{renderFooter}} for the corresponding server side function
#'   and examples.
#' @family footer outputs
#' @export
footerOutput <- function(outputId) {
  moduleOutput(outputId = outputId, tag = tags$footer)
}


#' Create dynamic footer (server side)
#'
#' @inheritParams shiny::renderUI
#'
#' @seealso \code{\link{footerOutput}} for the corresponding client side function
#'   and examples.
#' @family footer outputs
#' @export
renderFooter <- shiny::renderUI



