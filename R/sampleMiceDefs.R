#' Sample miceDefs object built off of iris dataset.
#' Included so examples don't run for too long.
#'
#' @source 
#' set.seed(1991)
#' data(iris)
#' ampIris <- amputeData(iris)
#' sampleMiceDefs <- miceRanger(
#'   ampIris
#'   ,m=3
#'   ,maxiter=3
#'   ,vars=list(
#'     Petal.Width = c("Sepal.Length","Sepal.Width","Petal.Length","Species")
#'     , Species = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
#'   )
#' )
#' @format A miceDefs object. See ```?miceRanger``` for details.
#' @examples
#' \dontrun{
#'  sampleMiceDefs
#' }
"sampleMiceDefs"