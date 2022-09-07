#' Plot indicator
#'
#' @description Plot indicator
#'
#' @param indicator_data a time series of the indicator data, with year and value as column names. Value should be in USD
#' @param revenue revenue in USD
#' @param revenue_year the year that the revenue data is calculated for
#' @return A ggplot object
#' @importFrom ggplot2 ggplot theme_minimal theme
#' @export
#'
#' @examples
#'
#'


plot_indicator <- function(indicator_data, revenue, revenue_year)
{

  revenue_data <- data.frame(year = c(revenue_year-1,revenue_year),
                             value = c(filter(indicator_data,year == revenue_year-1) %>%
                                         pull(value),
                                       revenue+filter(indicator_data,year == revenue_year) %>%
                               pull(value)),
                             label = c('','Potential spend with carbon revenue'))
p <- ggplot(indicator_data,aes(x = year, y = value))+
  geom_line()+
  geom_line(data = revenue_data,aes(x = year, y = value),lty = 2,col = '#cd2973')+
  geom_text(data = revenue_data,aes(x = year, y = value,label = label),
            hjust = 1.1,,col = '#cd2973')+
  theme_minimal(base_size = 16)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  labs(x = 'Year',y = 'Value (million USD)')
  scale_y_continuous(limits = c(min(indicator_data$value),max(indicator_data$value)+revenue))
return(p)
}
