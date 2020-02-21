#' Get Variable Imputations
#' 
#' These functions exist solely to get at the imputed data for a
#' specific dataset and variable.
#' 
#' @name getVarImps
#' @description Returns imputations for the specified datasets and variable.
#' @param x A \code{miceDefs} or \code{impDefs} object.
#' @param datasets The datasets to return. Can be a number, of a numeric vector.
#' @param var The variable to return the imputations for.
#' @return A matrix of imputations for a single variable. 
#' Each column represents a different dataset.
#' @examples
#' data("sampleMiceDefs")
#' getVarImps(sampleMiceDefs,var="Petal.Width")
#' @export
getVarImps <- function(x,datasets,var) UseMethod("getVarImps",x)

#' @export
getVarImps.default <- function(x,datasets,var) {
  stop("x must be of miceDefs or impDefs class.")
}

#' @method getVarImps miceDefs
#' @export
getVarImps.miceDefs <- function(
    x
  , datasets = 1:x$callParams$m
  , var
) {
  if (missing(var)) stop("Please provide a variable with the 'var' parameter")
  return(
    as.matrix(
      sapply(
          x$finalImps[datasets]
        , function(i) i[[var]]
      )
    )
  )
}

#' @method getVarImps impDefs
#' @export
getVarImps.impDefs <- function(
    x
  , datasets = x$callParams$datasets
  , var
) {
  if (missing(var)) stop("Please provide a variable with the 'var' parameter")
  return(
    as.matrix(
      sapply(
        x$imputedData[paste0("Dataset_",datasets)]
        , function(i) {
          i[x$naWhere[,var],var,with=FALSE][[1]]
        }
      )
    )
  )
}
