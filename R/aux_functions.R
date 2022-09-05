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
#' @param data_dor directory containing raw world bank and indicator data. Named world_bank_raw.csv and indicators.csv, respectively
#' @param year The year to get the data from
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

get_wb_indicators <- function(year,data_dir = 'data-raw')
{
  initial_indicator_set = read_csv(paste(data_dir,'indicators.csv',sep = '/'))$code
  world_bank_raw = read_csv(paste(data_dir,'world_bank_raw.csv',sep = '/'),skip = 3)

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


#' Update indicator-question template
#'
#' @description Write a blank template of expected scores for a given set of indicators and questions
#'
#' @param dir directory containing the files indicators.csv and questions.csv
#' @return A csv file 'indicator_question_template.csv' written to data-raw
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom tidyr expand_grid
#' @export
#'
#' @examples
#'
#'
#' update_indicator_question_template()
#'

update_indicator_question_template <- function(dir = 'data-raw')
{
  indicators <- read_csv(paste(dir,'indicators.csv',sep = '/'))
  questions <- read_csv(paste(dir,'questions.csv',sep = '/'))

  indicator_survey_matrix <- matrix(NA,nrow = nrow(indicators),ncol = nrow(questions))
  rownames(indicator_survey_matrix) <- str_remove_all(indicators$name,',')
  colnames(indicator_survey_matrix) <- str_remove_all(questions$name,',')

  write.csv(indicator_survey_matrix,paste(dir,'indicators_survey_template.csv',sep = '/'), na = '',quote = FALSE)
}

