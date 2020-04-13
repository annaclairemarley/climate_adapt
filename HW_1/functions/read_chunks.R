#' read_chunks
#' function to read in date and data from separate files & join to one dataframe
#'
#' @param folder name of folder in your working directory
#' @return dataframe
#' @export
#'
#' @examples read_chunks("data", fil_pattern = ".*nn_chrps_precip_.*", variable = "precipitation")

read_chunks = function(folder, fill_pattern = "*nn_chrps_precip_*", variable = "precipitation") {
  df = NULL
  for (file in list.files(folder, pattern = fill_pattern)) {
    readFile = read_csv(paste(folder, file, sep="/"), col_names=c("date", variable), skip = 1)
    if (is.null(df)) { # don't merge the first one
      df = readFile
    } else {
      df = rbind(df, readFile) # bind the files together
    }
  }
  return(df)
}