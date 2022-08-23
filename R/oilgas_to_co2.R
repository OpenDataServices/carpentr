#' Convert oil and gas production to CO2 emissions
#'
#' @description Convert oil and gas production to CO2 emissions
#'
#' @param production_data generated from read_eiti_production
#'
#' @return A numeric estimate of CO2 emissions
#'
#'
#' @export
#'
#' @examples
#'
#'
#' sn17 <- read_eiti_production("SN",2017)
#' oilgas_to_co2(sn17)

oilgas_to_co2 <- function(production_data)
{
  kggj_oil <- 87
  kggg_gas <- 63
  gjm3_oil <- 45
  gjm3_gas <- 10

  oil_prod <- as.numeric(production_data[production_data$label == "Oil, volume","value"])
  gas_prod <- as.numeric(production_data[production_data$label == "Gas, volume","value"])

  co2_oil <- oil_prod*gjm3_oil
  co2_gas <- gas_prod*gjm3_gas

  out <- (co2_oil+co2_gas)/1000

  return(out)
}
