#' @importFrom DescTools CramerV TschuprowT Phi YuleY YuleQ
#' @importFrom stats chisq.test
getFactCorrMetric <- function(met) {
  if(met == "CramerV") {
    return(
      list(
          corrMet = function(a,b) DescTools::CramerV(a,b,correct = TRUE)
        , ylb = "\nCramers V"
      )
    )
  } else if (met == "Chisq") {
    return(
      list(
        corrMet = function(a,b) stats::chisq.test(a,b)$statistic
        , ylb = "\nChi Sq Stat"
      )
    )
  } else if (met == "TschuprowT") {
    return(
      list(
        corrMet = function(a,b) DescTools::TschuprowT(a,b,correct = TRUE)
        , ylb = "\nTschuprows T"
      )
    )
  } else if (met == "Phi") {
    return(
      list(
          corrMet = function(a,b) {
            if (length(unique(c(a,b))) > 2) stop("Phi requires 2 unique levels.")
            DescTools::Phi(a,b)
          }
        , ylb = "\nPhi"
      )
    )
  } else if (met == "YuleY") {
    return(
      list(
        corrMet = function(a,b) {
          if (length(unique(c(a,b))) > 2) stop("YuleY requires 2 unique levels.")
          DescTools::YuleY(a,b)
        }
        , ylb = "\nYules Y"
      )
    )
  } else if (met == "YuleQ") {
    return(
      list(
        corrMet = function(a,b) {
          if (length(unique(c(a,b))) > 2) stop("YuleQ requires 2 unique levels.")
          DescTools::YuleQ(a,b)
        }
        , ylb = "\nYules Q"
      )
    )
  } else stop("Unknown factCorrMetric parameter")
    
}

#' @importFrom stats cor
getNumbCorrMetric <- function(met) {
  if(met == "pearson") {
    return(
      list(
        corrMet = function(a,b) stats::cor(a,b,method="pearson")
        , ylb = "\nPearsons r"
      )
    )
  } else if (met == "spearman") {
    return(
      list(
        corrMet = function(a,b) stats::cor(a,b,method="spearman")
        , ylb = "\nSpearmans Rho"
      )
    )
  } else if (met == "kendall") {
    return(
      list(
        corrMet = function(a,b) stats::cor(a,b,method="kendall")
        , ylb = "\nKendalls Tau"
      )
    )
  } else if (met == "Rsquared") {
    return(
      list(
          corrMet = function(a,b) stats::cor(a,b,method="pearson")^2
        , ylb = "\nR-Squared"
      )
    )
  } else stop("Unknown numbCorrMetric parameter")
  
}

