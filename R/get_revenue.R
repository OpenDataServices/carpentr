#' Estimate carbon pricing revenue
#'
#' @description Estimate carbon pricing revenue
#'
#' @param carbonprice carbon price per metric tonne in USD
#' @param emissions carbon emissions in metric tonnes
#'
#' @return A numeric revenue estimate
#'
#' @export
#'
#' @examples
#'
#' get_revenue(35,1000000)

get_revenue <- function(carbonprice,emissions)
{
  out <- carbonprice*emissions*0.9
  return(out)
}
