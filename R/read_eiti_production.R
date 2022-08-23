#' Read in EITI production data
#'
#' @description Read EITI production data for a given country and year
#'
#' @param country 2 letter country code
#' @param year as integer
#'
#' @return A data frame with one row for oil production and one for gas production
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#'
#' @export
#'
#' @examples
#'
#' read_eiti_production("SN",2017)

read_eiti_production <- function(country,year) {
  url <- paste0("https://eiti.org/api/v2.0/indicator_value?_format=json&year=",year,"&country=",country)
  res <- GET(url)
  out <- fromJSON(rawToChar(res$content))$data
  out <- filter(out,indicator.label %in% (c("Oil, volume","Gas, volume")),
                indicator.parent == "6") %>%
    select(label,value,unit)
  return(out)
}
