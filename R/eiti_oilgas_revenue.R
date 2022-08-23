#' Estimate carbon revenue for a given EITI country and year and carbon price
#'
#' @description Estimate carbon revenue for a given EITI country and year and carbon price
#'
#' @param country 2 letter country code
#' @param year as integer
#' @param carbonprice carbon price per metric tonne in USD
#'
#' @return A numeric revenue estimate
#'
#' @export
#'
#' @examples
#'
#' eiti_oilgas_revenue("SN",2017,35)

eiti_oilgas_revenue <- function(country, year, carbonprice){
  oilgas_production <- read_eiti_production(country = country,
                                            year = year)
  co2 <- oilgas_to_co2(oilgas_production)
  revenue <- get_revenue(co2,carbonprice)
  return(revenue)
}
