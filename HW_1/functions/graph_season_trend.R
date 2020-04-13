#' graph_season_trend
#'
#' @param df dataframe of average climate trend by year
#' @param climate_data the y axis column name, make sure you do df$column_name
#' @param ylabel what you want to call the y axis on the graph
#'
#' @return graph of time series with a linear model fitted
#' @export
#'
#' @examples
graph_season_trend = function(df, climate_data, ylabel = ""){

  # graph time series and add linear trendline 
  plot <- ggplot(df, aes(x = year, y = climate_data)) +
    geom_line() +
    labs(
        x = "Year",
        y = sprintf("%s", ylabel)
      ) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_x_continuous(expand = c(0,0)) +
      theme_bw()
  
  return(plot)
  
}