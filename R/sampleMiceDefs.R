#' Sample miceDefs object built off of iris dataset.
#' Included so examples don't run for too long.
#'
#' @source 
#' set.seed(1991)
#' data(iris)
#' ampIris <- amputeData(iris,cols = c("Petal.Width","Species"))
#' sampleMiceDefs <- miceRanger(
#'   ampIris
#'   ,m=3
#'   ,maxiter=3
#'   ,vars=c("Petal.Width","Species")
#' )
#' @format A miceDefs object. See ```?miceRanger``` for details.
#' @examples
#' \dontrun{
#'  sampleMiceDefs
#' }
"sampleMiceDefs"