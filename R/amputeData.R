#' @title amputeData
#' @description Randomly amputes data (MCAR).
#' @param data The data to be amputed
#' @param perc A scalar. The percentage (0-1) to be amputed.
#' @param cols The columns to ampute.
#' @importFrom data.table := .SD setDT
#' @return The same dataset with random values in \code{cols} set to NA.
#' @examples
#' data(iris)
#' head(iris,10)
#' 
#' ampIris <- amputeData(iris)
#' head(ampIris,10)
#' @export
amputeData <- function(
    data
  , perc = 0.10
  , cols = names(data)
)
{
  # Should we copy the entire dataset? 
  data <- copy(data)
  
  # Necessary if data was not a data.table.
  setDT(data)
  
  r <- round(nrow(data)*perc)

  data[,(cols) := lapply(.SD,function(x){
    x[sample(length(x),size=r)] <- NA
    return(x)
  }),.SDcols=cols]
  
  return(data)
  
}

