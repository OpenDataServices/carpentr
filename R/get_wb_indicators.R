#' Get world bank indicators
#'
#' @description Get world bank indicators, and transform values in USD per capita and in %GDP to totals in USD
#'
#' @param data_dir directory containing raw world bank and indicator data. Named world_bank_raw.csv and indicators.csv, respectively
#' @param years A numeric vector of the years to get the data from
#' @return A dataframe of cleaned indicators
#' @importFrom dplyr select filter mutate pull
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#'
#' get_wb_indicators(2017)
#'
#'

get_wb_indicators <- function(years,data_dir = 'data-raw')
{

  years <- as.character(years)
  initial_indicator_set <- read_csv(paste(data_dir,'indicators.csv',sep = '/'))$code
  world_bank_raw <- read_csv(paste(data_dir,'world_bank_raw.csv',sep = '/'),skip = 3)

  wide_data <- world_bank_raw[world_bank_raw$`Indicator Code` %in% initial_indicator_set,
                              c('Indicator Code','Indicator Name',years)] %>%
    rename(code = `Indicator Code`,
           name = `Indicator Name`) %>%
    mutate(name = str_remove_all(name,','))


  pop <- world_bank_raw %>%
    filter(`Indicator Code` == 'SP.POP.TOTL')
  pop_wide <- pop[,years]

  gdp <- world_bank_raw %>%
    filter(`Indicator Code` == 'NY.GDP.MKTP.CD')
  gdp_wide <- gdp[,years]

  if(length(years) >1)
  {
    long_data <- wide_data %>%
      pivot_longer(cols = -c(code,name),names_repair = "minimal",names_to = 'year') %>%
      mutate(year = as.numeric(year))

    pop_long <- pivot_longer(pop_wide,everything(),names_to = 'year',names_repair = "minimal",values_to = 'pop') %>%
      mutate(year = as.numeric(year))
    gdp_long <- pivot_longer(gdp_wide,everything(),names_to = 'year',names_repair = "minimal",values_to = 'gdp') %>%
      mutate(year = as.numeric(year))

  } else{

    long_data <- rename(wide_data,value = years)
    pop_long <- rename(pop_wide,value = years)
    gdp_long <- rename(gdp_wide,value = years)
  }



  cleaned_indicators <- long_data %>%
    left_join(pop_long) %>%
    left_join(gdp_long) %>%
    mutate(value = case_when(str_detect(code,'.ZS') ~ (value/100)*gdp,
                             TRUE ~ value)) %>%
    mutate(value = case_when(str_detect(code,'.PC.') ~ (value)*pop,
                             TRUE ~ value)) %>%
      select(-pop,-gdp)

  return(cleaned_indicators)

}
