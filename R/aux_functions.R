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


#' Calculate Euclidean distance
#'
#' @description Calculate Euclidean distance
#'
#' @param a A numeric vector
#' @param b A numeric vector
#' @return A numeric Euclidean distance
#' @export
#'
#' @examples
#'
#' x <- c(4,5,7)
#' y <- c(9,12,3)
#' euclidean(x,y)

euclidean <- function(a, b){
  out <- sqrt(sum((a - b)^2))
  return(out)
}
