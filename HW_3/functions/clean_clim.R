#' clean_clim
#'
#' clean climate data from climate projections into usuable format for avocado model
#'
#' @param df tmin, tmax, or precip data
#' @param type which kind of df is its
#'
#' @return
#' @export
#'
#' @examples

clean_clim = function(df, type = "tmax"){
  
  if (type == "tmax") {
    
    # clean and summarize by correct month
    clean_df <- df %>% 
      mutate(year = year(time)) %>% 
      mutate(month = month(time)) %>% 
      dplyr::select(year, month, mean) %>% 
      filter(month == 8) %>% 
      rename(tmax = mean) %>% 
      dplyr::select(year, tmax) %>% 
      mutate(tmax = tmax - 273.15) # convert to celsius
    
  } else if (type == "tmin") {
    
    # clean and summarize by correct month
    clean_df <- df %>% 
      mutate(year = year(time)) %>% 
      mutate(month = month(time)) %>% 
      dplyr::select(year, month, mean) %>% 
      filter(month == 5) %>% 
      rename(tmin = mean) %>% 
      dplyr::select(year, tmin) %>% 
      mutate(tmin = tmin - 273.15) # convert to celsius
    
  } else {
    
    clean_df <- df %>% 
      mutate(year = year(time)) %>% 
      mutate(month = month(time)) %>% 
      group_by(year, month) %>% 
      summarize(precip = sum(precip)) %>% 
      filter(month == 10) %>% 
      dplyr::select(year, precip)
    
  }
  
  return(clean_df)
  
}