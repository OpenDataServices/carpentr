#' Plot indicator
#'
#' @description Plot indicator
#'
#' @param indicator_data a time series of the indicator data, with year and value as column names. Value should be in USD
#' @param revenue revenue in USD
#' @param revenue_year the year that the revenue data is calculated for
#' @return A ggplot object
#' @importFrom ggplot2 ggplot theme_minimal theme
#' @importfrom dplyr %>% filter pull
#' @export
#'
#' @examples
#'
#'


plot_indicator <- function(indicator_data, revenue, revenue_year)
{


  if('lab' %in% colnames(indicator_data))
  {
    nlabs <- length(unique(indicator_data$lab))

    revenue_data <- data.frame(year = rep(c(revenue_year-1,revenue_year),each = nlabs),
                               value = c(filter(indicator_data,year == revenue_year-1) %>%
                                           pull(value),
                                         revenue+filter(indicator_data,year == revenue_year) %>%
                                           pull(value)),
                               lab = rep(unique(indicator_data$lab),2)) %>%
      filter(!is.na(value)) %>%
      mutate(year = as.Date(paste0(year,'-01-01')))
  } else
  {
    revenue_data <- data.frame(year = c(revenue_year-1,revenue_year),
                               value = c(filter(indicator_data,year == revenue_year-1) %>%
                                           pull(value),
                                         revenue+filter(indicator_data,year == revenue_year) %>%
                                           pull(value)),
                               label = c('','Potential value with carbon revenue')) %>%
      filter(!is.na(value)) %>%
      mutate(year = as.Date(paste0(year,'-01-01')))
  }


  indicator_data <- indicator_data %>%
    filter(!is.na(value)) %>%
    mutate(year = as.Date(paste0(year,'-01-01')))

p <- ggplot(indicator_data,aes(x = year, y = value))+
  geom_line(aes(lty = 'Without carbon pricing revenue',col = 'Without carbon pricing revenue'))+
  geom_line(data = revenue_data,aes(x = year, y = value,lty = 'With carbon pricing revenue',col = 'With carbon pricing revenue'))+
  theme_minimal(base_size = 14)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
legend.position = 'right')+
  labs(x = '',y = 'Value (million USD)',color = 'type',linetype = 'type')+
  scale_color_manual(values = c('#cd2973','black'))+
  scale_linetype_manual(values = c(2,1))
return(p)
}
