#' Transform Point Decimals (R) to Comma Decimals (Excel)
#'
#' This function takes a dataframes, scans the columns containing decimal
#' numbers and transforms the point into a comma.
#'
#' @param A dataframe
#' @return The transformed dataframe
R2Excel <- function(dataframe) {
  for (i in 1:length(colnames(dataframe))) {
    if (is.numeric(dataframe[, i]) &&
          any(dataframe[, i] != floor(dataframe[, i]))) {
      dataframe[, i] <- gsub("\\.", ",", dataframe[, i])
    }
  }
  dataframe
}