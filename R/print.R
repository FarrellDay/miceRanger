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
  cat("\nClass:          miceDefs\n")
  cat("Datasets:      ",m,"\n")
  cat("Iterations:    ",maxiter,"\n")
  cat("Total Seconds: ",imputationTime,"\n")
  cat("Imputed Cols:  ",length(x$callParams$vars),"\n")
  cat("Estimated Time per Additional Iteration is",round(imputationTime/maxiter*m),"Seconds","\n")
  cat("Estimated Time per Additional Dataset is",round(imputationTime/m*maxiter),"Seconds","\n\n")
  cat("For additional metrics, see the different plotting functions.")
  invisible(x)
}
