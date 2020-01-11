#' @title plotVarConvergence
#' @description Create a impDefs object, which contains information about the imputation process.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param vars the variables you want to plot. Default is to plot all variables. Can be a vector of
#' variable names, or one of 'allNumeric' or 'allCategorical'
#' @param ... options passed to \code{grid.arrange()}
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom ggplot2 ggplot geom_point ylab aes theme aes_string element_blank geom_errorbar
#' @importFrom stats sd
#' @importFrom ggpubr ggarrange theme_classic2
#' @importFrom DescTools Entropy
#' @return nothing.
#' @examples 
#' data("sampleMiceDefs")
#' plotVarConvergence(sampleMiceDefs)
#' @export
plotVarConvergence <- function(
    miceObj
  , vars = names(miceObj$callParams$vars)
  , ...
) {
  
  if (miceObj$callParams$maxiter == 1) stop("There is only 1 iteration, need at least 2 iterations to plot convergence.")
  
  if (vars[[1]] == 'allCategorical') vars <- names(miceObj$newClasses[miceObj$newClasses == "factor"])
  if (vars[[1]] == 'allNumeric') vars <- names(miceObj$newClasses[miceObj$newClasses != "factor"])
  
  selTheme <- theme_classic2()
  m <- miceObj$callParams$m
  maxiter <- miceObj$callParams$maxiter
  
  pList <- lapply(
      vars
    , function(var) {
      #var <- vars[[5]]
      
      # Type of plot depends on variable type.
      varClass <- if (miceObj$newClasses[var] == "factor") "factor" else "numeric"
      
      #Extract the mean and sd in each dataset at each iteration.
      getMetric <- function(metric) {
        sapply(
          miceObj$allImps
          , function(datSet) {
            sapply(
              datSet
              , function(iter) {
                metric(iter[[var]])
              }
            )
          }
        )
      }
      
      if (varClass == "factor") {
        varMetric <- data.table(getMetric(function(x) Entropy(table(x))))
        centerMetric <- data.table(getMetric(function(x) sort(table(x))[[1]]/length(x)))
      } else if (varClass == "numeric") {
        varMetric <- data.table(getMetric(sd))
        centerMetric <- data.table(getMetric(mean))
      }
      
      varMetric$iteration <- 1:miceObj$callParams$maxiter
      centerMetric$iteration <- 1:miceObj$callParams$maxiter
      
      varMetric <- melt(varMetric,id.vars = "iteration")
      centerMetric <- melt(centerMetric,id.vars = "iteration")
      
      return(
        arrangeGrob(
          ggarrange(
              ggplot(varMetric,aes_string(x="iteration",y="value",group="variable")) +
                geom_line() +
                ylab(if (varClass == "factor") "Entropy" else "SD") +
                selTheme +
                theme(
                    axis.title.x = element_blank()
                  , axis.text.x = element_blank()
                )
            , ggplotGrob(
                  ggplot(centerMetric,aes_string(x="iteration",y="value",group="variable")) +
                    geom_line() +
                    ylab(if (varClass == "factor") "Mode Perc" else "Mean") +
                    selTheme
            ),align="v",nrow=2,heights=c(0.8,1)
          ),left=var
        )
      )
    }
  )
  
  grid.arrange(grobs = pList,...)

}
