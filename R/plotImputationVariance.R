#' @title plotImputationVariance
#' @description plots the distribution of the difference between datasets of the imputed values.
#' For categorical variables, the distribution of the number of distinct levels imputed for each sample
#' is shown next to the expected hypergeometric distribution, if the imputation was completely random.
#' For numeric variables, the density of the standard deviation (between datasets) of imputations is 
#' plotted. The shaded area represents the samples that had a standard deviation lower than the total
#' nonmissing standard deviation for the original data.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param vars the variables you want to plot. Default is to plot all variables. Can be a vector of
#' variable names, or one of 'allNumeric' or 'allCategorical'
#' @param title optional title for the arranged grob.
#' @param monteCarloSimulations The number of simulations to run to determine the distribution of
#' unique categorical levels drawn if the draws were completely random.
#' @param ... additional parameters passed to ggarrange when combining numeric and categorical
#' plots.
#' @importFrom data.table data.table
#' @importFrom ggplot2 position_nudge geom_area xlab ylab ggtitle scale_x_discrete geom_vline
#' @return nothing.
#' @examples 
#' data("sampleMiceDefs")
#' plotImputationVariance(sampleMiceDefs)
#' @export
plotImputationVariance <- function(
    miceObj
  , vars = names(miceObj$callParams$vars)
  , title=NULL
  , monteCarloSimulations = 10000
  , ...
) {
  
  # require(ggplot2)
  # require(ggpubr)
  
  if (vars[[1]] == 'allCategorical') vars <- names(miceObj$newClasses[miceObj$newClasses == "factor"])
  if (vars[[1]] == 'allNumeric') vars <- names(miceObj$newClasses[miceObj$newClasses != "factor"])
  
  if (miceObj$callParams$m == 1) stop("There is only 1 dataset, cannot plot variance between datasets.")
  if (miceObj$callParams$m == 2) message("There are only 2 datasets, plotting density of difference between imputed values.")
  
  facVars <- miceObj$newClasses[vars][miceObj$newClasses[vars] == "factor"]
  numVars <- miceObj$newClasses[vars][miceObj$newClasses[vars] != "factor"]
  
  
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
      v <- names(facVars)[[1]]
      if(sum(miceObj$naWhere[,v]) ==1 ) stop(paste0(v," was only imputed once. Cannot plot density of the variance of a single imputation."))
      imps <- as.data.table(lapply(miceObj$finalImps,function(x) x[[v]]))
      
      toPlot <- getVar(
          finalImps = imps
        , miceObj$callParams$m
        , v=v
      )
      vec <- miceObj$data[!miceObj$naWhere[,v],get(v)]
      numLvls <- length(levels(vec))
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
          origDist <- sd(miceObj$data[!miceObj$naWhere[,v],get(v)])
          qtile <- round(mean(toPlot <= origDist)*100)
          dens <- density(toPlot)
          origDist <- dens$x[FNN::knnx.index(dens$x,origDist,k=1)]
          dat <- data.table(
              x = dens$x
            , y = dens$y
            , shaded = dens$x <= origDist
          )
          
          p <- ggplot(dat,aes_string("x","y")) + 
            geom_line() +
            geom_area(data=dat[get("shaded")==TRUE],fill="grey",alpha=0.75) +
            xlab(if(miceObj$callParams$m == 2)"Difference"else"Standard Deviation") +
            ylab("Density") +
            ggtitle(paste0(v," - Q=",qtile)) +
            geom_vline(xintercept=origDist,linetype="dashed")
          
          return(p)
        
        }
      )
    } else numList <- NULL
  
  if (length(facList) == 0) {
    ggarrange(plotlist=numList,...)
  } else if (length(numList) == 0) {
    ggarrange(plotlist=facList,...)
  } else {
    ggarrange(
      plotlist=list(
        if (length(facList) > 0) ggarrange(plotlist=facList) else NULL
        , if (length(numList) > 0) ggarrange(plotlist=numList) else NULL
      )
      , ...
    )
  }
  
}
utils::globalVariables(c("x","..count.."))
