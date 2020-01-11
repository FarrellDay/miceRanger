#' @title getVarImps
#' @description Returns all of the imputations for the specified
#' datasets and variable.
#' @param miceObj A miceDefs object created by \code{miceRanger}.
#' @param datasets The datasets to return. Can be a number, of a numeric vector.
#' @param var The variable to return the imputations for.
#' @examples
#' data("sampleMiceDefs")
#' getVarImps(sampleMiceDefs,"Sepal.Width")
#' @export
getVarImps <- function(
    miceObj
  , datasets = 1:miceObj$callParams$m
  , var
) {
  as.matrix(sapply(miceObj$finalImps[datasets],function(x) x[[var]]))
}
