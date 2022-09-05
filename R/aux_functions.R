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


#' Get world bank indicators
#'
#' @description Get world bank indicators, and transform values in USD per capita and in %GDP to totals in USD
#'
#' @param world_bank_raw Raw world bank development indicator data
#' @param year The year to get the data from
#' @param indicator_set a vector of indicator codes to filter
#' @return A dataframe of cleaned indicators
#' @importFrom dplyr select filter mutate pull
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' initial_indicator_set <- c(
#' 'GC.TAX.TOTL.GD.ZS',
#' 'MS.MIL.XPND.CD',
#' 'NY.ADJ.AEDU.CD',
#' 'SH.XPD.CHEX.PC.CD',
#' 'ST.INT.XPND.CD',
#' 'NE.CON.GOVT.CD'
#' )
#'
#' get_wb_indicators(world_bank_raw,2017,initial_indicator_set)
#'
#'

get_wb_indicators <- function(world_bank_raw,year,initial_indicator_set)
{
  world_bank_raw[,"value"] <- world_bank_raw[,as.character(year)]

  selected_indicators <- world_bank_raw %>%
    select(code = `Indicator Code`,
           name = `Indicator Name`,
           value) %>%
    filter(code %in% initial_indicator_set)

  pop <- world_bank_raw %>%
    filter(`Indicator Code` == 'SP.POP.TOTL') %>%
    pull(value)

  gdp <- world_bank_raw %>%
    filter(`Indicator Code` == 'NY.GDP.MKTP.CD') %>%
    pull(value)

  cleaned_indicators <- selected_indicators %>%
    mutate(value = case_when(str_detect(code,'.ZS') ~ (value/100)*gdp,
                             TRUE ~ value)) %>%
    mutate(value = case_when(str_detect(code,'.PC.') ~ (value)*pop,
                             TRUE ~ value))

  return(cleaned_indicators)

}

