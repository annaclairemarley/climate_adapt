#' extract_months
#' 
#' CExtracts months wanted for analysis
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples

extract_months = function (df, wanted_months = c(11, 12, 1, 2, 3, 4)) {
  filteredDF <- df %>% 
    filter(month(date) %in% wanted_months)
  return(filteredDF)
}
