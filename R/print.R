#'Print a \code{miceDefs} object
#' 
#'@rdname print
#'@param x Object of class \code{miceDefs}
#'@param ... required to use S3 method
#'@return \code{NULL}
#'@method print miceDefs
#'@export
print.miceDefs <- function(x,...) {
  m <- x$callParams$m
  maxiter <- x$callParams$maxiter
  imputationTime <- x$imputationTime
  cat("\nClass:\t\t miceDefs\n")
  cat("Datasets:\t",m,"\n")
  cat("Iterations:\t",maxiter,"\n")
  cat("Total Seconds:\t",imputationTime,"Seconds","\n")
  cat("Imputed Cols:\t",length(x$callParams$vars),"\n")
  cat("Estimated Time per Additional Iteration is",round(imputationTime/m),"Seconds","\n")
  cat("Estimated Time per Additional Dataset is",round(imputationTime/maxiter),"Seconds","\n\n")
  cat("For additional metrics, see the different plotting functions.")
  invisible(x)
}
