#' @title plotImputationVariance
#' @description Plots the distribution of the difference between datasets of the imputed values.
#' For categorical variables, the distribution of the number of distinct levels imputed for each sample
#' is shown next to the distribution of unique draws from that variable in the nonmissing data, given that
#' the draws were completely random.
#' For numeric variables, the density of the standard deviation (between datasets) of imputations is 
#' plotted. The shaded area represents the samples that had a standard deviation lower than the total
#' nonmissing standard deviation for the original data.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param vars the variables you want to plot. Default is to plot all variables. Can be a vector of
#' variable names, or one of 'allNumeric' or 'allCategorical'
#' @param monteCarloSimulations The number of simulations to run to determine the distribution of
#' unique categorical levels drawn if the draws were completely random.
#' @param ... additional parameters passed to \code{ggarrange()}.
#' @importFrom data.table data.table
#' @importFrom ggplot2 position_nudge geom_area xlab ylab ggtitle scale_x_discrete geom_vline
#' @return an object of class \code{ggarrange}.
#' @examples 
#' data("sampleMiceDefs")
#' plotImputationVariance(
#'   sampleMiceDefs
#'   , monteCarloSimulations = 100
#' )
#' @export
plotImputationVariance <- function(
    miceObj
  , vars = names(miceObj$callParams$vars)
  , monteCarloSimulations = 10000
  , ...
) {

  # Get variable and class information from miceObj
  varn <- names(miceObj$callParams$vars)
  newClasses <- miceObj$newClasses[varn]
  m <- miceObj$callParams$m
  
  # Change vars if needed
  if (vars[[1]] == 'allCategorical' & length(vars) == 1) {
    vars <- names(newClasses[newClasses == "factor"])
  } else if (vars[[1]] == 'allNumeric' & length(vars) == 1) {
    vars <- names(newClasses[newClasses != "factor"])
  }
  
  # Checks
  if (m == 1) stop("There is only 1 dataset, cannot plot variance between datasets.")
  if (m == 2) message("There are only 2 datasets, plotting density of difference between imputed values.")
  if (any(!vars %in% varn)) stop("Specified variables were not imputed.")
  
  # Only keep class information about variables we are plotting:
  newClasses <- newClasses[vars]
  facVars <- newClasses[newClasses == "factor"]
  numVars <- newClasses[newClasses != "factor"]
  
  
  getVar <- function(finalImps,m,v) {
    if (miceObj$newClasses[v] == "factor") {
      toPlot <- apply(finalImps,MARGIN=1,function(x) length(unique(x)))
    } else {
      if (m == 2) {
        toPlot <- abs(finalImps$Dataset_1 - finalImps$Dataset_2)
      } else {
        toPlot <- apply(finalImps,MARGIN=1,sd)
      }
    }
    return(toPlot)
  }
  if (length(facVars) > 0) {
  
    facList <- lapply(
      names(facVars)
    , function(v) {
      if(sum(miceObj$naWhere[,v]) == 1) stop(paste0(v," was only imputed once. Cannot plot density of the variance of a single imputation."))
      imps <- as.data.table(lapply(miceObj$finalImps,function(x) x[[v]]))
      
      toPlot <- getVar(
          finalImps = imps
        , miceObj$callParams$m
        , v=v
      )
      vec <- miceObj$data[!miceObj$naWhere[,v],get(v)]
      numLvls <- length(unique(vec))
      psibleUnqDraws <- pmin(miceObj$callParams$m,numLvls)
        
      monteCarlo = sapply(
          1:monteCarloSimulations
        , function(x) {
          length(unique(sample(vec,size=miceObj$callParams$m,replace = TRUE)))
        }
      )

      p <- ggplot() + 
        geom_bar(data=data.frame(x=factor(toPlot,levels = 1:psibleUnqDraws)),aes(x=x,y = (..count..)/sum(..count..),fill="Imputed Values"),alpha=0.75,position = position_nudge(x = 0.2),width=0.4) +
        geom_bar(data=data.frame(x=monteCarlo),aes(x=x,y = (..count..)/sum(..count..),fill="Random Sampling"),alpha=0.75,position = position_nudge(x = -0.2),width=0.4) +
        ggtitle(v) +
        xlab("Distinct Imputations") +
        ylab("Percent") +
        scale_x_discrete(drop=FALSE)
      p$labels$fill <- "Source"
      
      return(p)
        
      }
    )
    
    facList <- ggarrange(plotlist = facList,common.legend = TRUE,legend="bottom")
    
    } else facList <- NULL
  
  
    if (length(numVars) > 0) {
      numList <- lapply(
        names(numVars)
        , function(v) {
          if(sum(miceObj$naWhere[,v]) ==1 ) stop(paste0(v," was only imputed once. Cannot plot density of the variance of a single imputation."))
          imps <- as.data.table(lapply(miceObj$finalImps,function(x) x[[v]]))
          
          toPlot <- getVar(
            finalImps = imps
            , miceObj$callParams$m
            , v=v
          )
          
          # Find datapoint closest to quantile.
          origDist <- sd(miceObj$data[!miceObj$naWhere[,v],get(v)])
          qtile <- round(mean(toPlot <= origDist)*100)
          dens <- density(toPlot)
          origDist <- dens$x[FNN::knnx.index(dens$x,origDist,k=1)]
          dat <- data.table(
              x = dens$x
            , y = dens$y
            , shaded = dens$x <= origDist
          )
          
          return(
            ggplot(dat,aes_string("x","y")) + 
              geom_line() +
              geom_area(data=dat[get("shaded")==TRUE],fill="grey",alpha=0.75) +
              xlab(if(miceObj$callParams$m == 2)"Difference"else"Standard Deviation") +
              ylab("Density") +
              ggtitle(paste0(v," - Q=",qtile)) +
              geom_vline(xintercept=origDist,linetype="dashed")
          )

        }
      )
      
      numList <- ggarrange(plotlist = numList)
      
    } else numList <- NULL
  
  # Since we are returning ggarrange instead of ggplot, we can't
  # pass NULL to ggarrange, or it will plot a blank space.
  if (length(numVars) == 0) {
    ggarrange(facList,...)
  } else if (length(facVars) == 0) {
    ggarrange(numList,...)
  } else {
    ggarrange(numList,facList,...)
  }
  
}
utils::globalVariables(c("x","..count.."))
