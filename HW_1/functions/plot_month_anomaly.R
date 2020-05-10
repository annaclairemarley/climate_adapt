
#' plot_month_anomaly
#'
#' @param df initial precipitation dataframe
#' @param months the month you want to create an average for
#' @param anom_year the year that youll calculate the average metric for (all years less than 1981)
#' @param title the title of your graph
#'
#' @return
#' @export
#'
#' @examples

source('../HW_1/functions/extract_months.R') # need to source other function 

plot_month_anomaly = function(df, months = c(12), anom_year = 1991, title = "December"){
  
  # pull month average precipitation for every water year
  month_precip <- extract_months(precip, wanted_months = months) %>% 
    rename(Date = date) %>% 
    addWaterYear() %>% 
    group_by(waterYear) %>% 
    summarize(precip_av = mean(precipitation))
  
  # determine the average precip for years less than or equal to the anom_year
  av_anom <- month_precip %>% 
    filter(waterYear <= anom_year) %>% 
    summarize(av_sample = mean(precip_av))
  
  # calculate anomaly for whole time series based off of the anomaly year average 
  anom <- month_precip %>% 
    mutate(mean = av_anom$av_sample) %>% 
    mutate(anomaly = precip_av - mean) %>% 
    mutate(sign = ifelse(anomaly < 0, "negative", "positive"))

  
  # graph it
  anom_plot <- ggplot(anom, aes(x = waterYear, y = anomaly)) + 
      geom_col(aes(fill = sign), show.legend = FALSE) +
      scale_fill_manual(values = c("negative" = "#df5e3d", "positive" = "#a6c39d")) +
      labs(
        x = "Water Year",
        y = "Anomaly",
        title = sprintf("%s", title)
      ) +
    geom_smooth(method = "lm", se = FALSE) +
      theme_classic()
  
  return(anom_plot)
}