#' @title plotCorrelations
#' @description Create a impDefs object, which contains information about the imputation process.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param vars the variables you want to plot. Default is to plot all variables. Can be a vector of
#' variable names, or one of 'allNumeric' or 'allCategorical'
#' @param factCorrMetric The correlation metric for categorical variables. Can be one of:
#' \itemize{
#'   \item {CramerV} Cramer's V correlation metric.
#'   \item {Chisq} Chi Square test statistic.
#'   \item {TschuprowT} Tschuprow's T correlation metric.
#'   \item {Phi} (Binary Variables Only) Phi coefficient.
#'   \item {YuleY} (Binary Variables Only) Yule's Y, also known as coefficient of colligation
#'   \item {YuleQ} (Binary Variables Only) Yule's Q, related to Yule's Y by Q=2Y/(1+Y^2)
#' }
#' @param numbCorrMetric The correlation metric for numeric variables. Can be one of:
#' \itemize{
#'   \item {pearson} Pearson's Correlation Coefficient
#'   \item {spearman} Spearman's Rank Correlation Coefficient
#'   \item {kendall} Kendall's Rank Correlation Coefficient
#'   \item {Rsquared} R-squared
#' }
#' @param ... Other arguments to pass to grid.arrange()
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom ggplot2 ggplot geom_point ylab aes theme aes_string geom_boxplot
#' @importFrom stats cor
#' @importFrom utils combn
#' @importFrom ggpubr ggarrange theme_classic2
#' @importFrom DescTools CramerV
#' @return nothing.
#' @examples 
#' data(iris)
#' ampIris <- amputeData(iris)
#' miceObj <- miceRanger(
#'   ampIris
#'   , m = 3
#'   , maxiter = 3
#'   , verbose=FALSE
#' )
#' plotCorrelations(miceObj)
#' @export
plotCorrelations <- function(
    miceObj
  , vars = miceObj$callParams$vars
  , factCorrMetric = "CramerV"
  , numbCorrMetric = "pearson"
  , ...
) {
  
  if (vars[[1]] == 'allCategorical') vars <- names(miceObj$newClasses[miceObj$newClasses == "factor"])
  if (vars[[1]] == 'allNumeric') vars <- names(miceObj$newClasses[miceObj$newClasses != "factor"])
  
  selTheme <- theme_classic2()
  m <- miceObj$callParams$m
  maxiter <- miceObj$callParams$maxiter
  
  if (m < 3) stop("Cannot plot correlations with 2 or less datasets. This functionality will be implemented at a later time.")
  
  # Define Correlation Metrics:
  catCorrMet <- getFactCorrMetric(factCorrMetric)
  numCorrMet <- getNumbCorrMetric(numbCorrMetric)
  
  pList <- lapply(
    vars
    , function(var) {
      #var <- vars[[1]]
      # Type of plot depends on variable type.
      if (miceObj$newClasses[var] == "factor") {
        varClass <- "factor"
        ylb <- paste0(var,catCorrMet$ylb)
      } else {
        varClass <- "numeric"
        ylb <- paste0(var,numCorrMet$ylb)
      }

      # Calculate correlation metric for every combination
      # of datasets at each iteration.
      iterFlipped <- as.data.table(
        sapply(
          1:maxiter
          , function(x) {
            #x <- 1
            mat <- apply(
              combn(1:m,m=2)
              , MARGIN=2
              , function(ind) {
                #ind <- combn(1:m,m=2)[,1]
                i <- ind[[1]]
                j <- ind[[2]]
                if(varClass == "factor") {
                  return(
                    catCorrMet$corrMet(
                        miceObj$allImps[[i]][[x]][[var]]
                      , miceObj$allImps[[j]][[x]][[var]]
                    )
                  )
                } else {
                  return(
                    numCorrMet$corrMet(
                        miceObj$allImps[[i]][[x]][[var]]
                      , miceObj$allImps[[j]][[x]][[var]]
                    )
                  )
                }
                
              }
            )
            return(mat)
          }
        )
      )
      
      setnames(iterFlipped,as.character(1:maxiter))
      iterFlipped <- melt(iterFlipped,measure.vars = names(iterFlipped))
      iterFlipped$variable <- factor(as.numeric(iterFlipped[,get("variable")]))
      
      return(
        ggplotGrob(
          ggplot(iterFlipped,aes_string(x="variable",y="value")) + 
            geom_boxplot() +
            ylab(ylb) +
            selTheme +
            xlab("Iteration")
        )
      )
    }
  )
  
  grid.arrange(grobs = pList,...)
  
}
