#' Run shiny carpentr app
#'
#' @description Run the shiny app
#'
#' @return An initialised shiny app
#' @export
#'
#' @examples
#'
#' shinycarpentr()

shinycarpentr <- function() {
  appDir <- system.file("shiny", "shinycarpentr", package = "carpentr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `carpentr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
