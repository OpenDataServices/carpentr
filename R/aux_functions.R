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

