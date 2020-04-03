#' miceRanger: Fast Imputation with Random Forests
#' 
#' Performs multiple imputation by chained random forests.
#' Returns a miceDefs object, which contains information about the imputation process.
#' 
#' @param data A data.frame or data.table to be imputed.
#' @param m The number of datasets to produce.
#' @param maxiter The number of iterations to run for each dataset.
#' @param vars Specifies which and how variables should be imputed. Can be specified in 3 different ways:
#' \itemize{
#'   \item {<missing>} If not provided, all columns will be imputed using all columns. If
#'   a column contains no missing values, it will still be used as a feature to impute missing columns.
#'   \item {<character vector>} If a character vector of column names is passed, these columns will
#'   be imputed using all available columns in the dataset. The order of this vector will determine the 
#'   order in which the variables are imputed.
#'  \item {<named list of character vectors>} Predictors can be specified for each variable with a named list. 
#'  List names are the variables to impute. Elements in the vectors should be features used to 
#'  impute that variable. The order of this list will determine the order in which the variables are imputed.
#' }
#' @param valueSelector How to select the value to be imputed from the model predictions. 
#' Can be "meanMatching", "value", or a named vector containing a mixture of those values.
#' If a named vector is passed, the names must equal the variables to be imputed specified in \code{vars}.
#' @param meanMatchCandidates Specifies the number of candidate values which are selected from in the 
#' mean matching algorithm. Can be either specified as an integer or a named integer vector for different 
#' values by variable. If a named integer vector is passed, the names of the vector must contain at a 
#' minimum the names of the numeric variables imputed using \code{valueSelector = "meanMatch"}.
#' @param returnModels Logical. Should the final model for each variable be returned? Set to \code{TRUE}
#' to use the \code{impute} function, which allows imputing new samples without having to run \code{miceRanger} again.
#' Setting to TRUE can cause the returned \code{miceDefs} object to take up a lot of memory. Use only if
#' you plan on using the \code{impute} function.
#' @param parallel Should the process run in parallel? Usually not necessary. This process will 
#' take advantage of any cluster set up when \code{miceRanger} is called.
#' @param verbose should progress be printed?
#' @param ... other parameters passed to \code{ranger()} to control forest growth.
#' @importFrom data.table as.data.table rbindlist setcolorder setnames copy setDT set
#' @importFrom utils tail
#' @importFrom ranger ranger
#' @importFrom stats predict
#' @importFrom crayon make_style
#' @import foreach
#' @return a miceDefs object, containing the following:
#' \item{callParams}{The parameters of the object.}
#' \item{data}{The original data provided by the user, cast to a data.table.}
#' \item{naWhere}{Logical index of missing data, having the same dimensions as \code{data}.}
#' \item{missingCounts}{The number of missing values for each variable}
#' \item{rawClasses}{The original classes provided in \code{data}}
#' \item{newClasses}{The new classes of the returned data.}
#' \item{allImps}{The imputations of all variables at each iteration, for each dataset.}
#' \item{allImport}{The variable importance metrics at each iteration, for each dataset.}
#' \item{allError}{The OOB model error for all variables at each iteration, for each dataset.}
#' \item{finalImps}{The final imputations for each dataset.}
#' \item{finalImport}{The final variable importance metrics for each dataset.}
#' \item{finalError}{The final model error for each variable in every dataset.}
#' \item{finalModels}{Only returned if \code{returnModels = TRUE}. A list of \code{ranger} random forests for each dataset/variable.}
#' \item{imputationTime}{The total time in seconds taken to create the imputations for the 
#'   specified datasets and iterations. Does not include any setup time.}
#'   
#' @section Vignettes:
#' 
#' It is highly recommended to visit the \href{https://github.com/farrellday/miceRanger}{GitHub README} 
#' for a thorough walkthrough of miceRanger's capabilities, as well as performance benchmarks.
#' 
#' Several vignettes are also available on \href{https://cran.r-project.org/package=miceRanger}{miceRanger's listing}
#' on the CRAN website.
#' \enumerate{
#'   \item \href{https://cran.r-project.org/package=miceRanger/vignettes/miceAlgorithm.html}{The MICE Algorithm}
#'   \item \href{https://cran.r-project.org/package=miceRanger/vignettes/usingMiceRanger.html}{Imputing Missing Data with miceRanger}
#'   \item \href{https://cran.r-project.org/package=miceRanger/vignettes/diagnosticPlotting.html}{Diagnostic Plotting}
#' }
#' 
#' @examples
#' #################
#' ## Simple Example
#' 
#' data(iris)
#' ampIris <- amputeData(iris)
#' 
#' miceObj <- miceRanger(
#'     ampIris
#'   , m = 1
#'   , maxiter = 1
#'   , verbose=FALSE
#'   , num.threads = 1
#'   , num.trees=5
#' )
#' 
#' \donttest{
#' ##################
#' ## Run in parallel
#' 
#' data(iris)
#' ampIris <- amputeData(iris)
#' 
#' library(doParallel)
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#' 
#' # Perform mice 
#' miceObjPar <- miceRanger(
#'     ampIris
#'   , m = 2
#'   , maxiter = 2
#'   , parallel = TRUE
#'   , verbose = FALSE
#' )
#' stopCluster(cl)
#' registerDoSEQ()
#' 
#' 
#' ############################
#' ## Complex Imputation Schema
#' 
#' data(iris)
#' ampIris <- amputeData(iris)
#' 
#' # Define variables to impute, as well as their predictors
#' v <- list(
#'   Sepal.Width = c("Sepal.Length","Petal.Width","Species")
#'   , Sepal.Length = c("Sepal.Width","Petal.Width")
#'   , Species = c("Sepal.Width")
#' )
#' 
#' # Specify mean matching for certain variables.
#' vs <- c(
#'   Sepal.Width = "meanMatch"
#'   , Sepal.Length = "value"
#'   , Species = "meanMatch"
#' )
#' 
#' # Different mean matching candidates per variable.
#' mmc <- c(
#'   Sepal.Width = 4
#'   , Species = 10
#' )
#' 
#' miceObjCustom <- miceRanger(
#'     ampIris
#'   , m = 1
#'   , maxiter = 1
#'   , vars = v
#'   , valueSelector = vs
#'   , meanMatchCandidates = mmc
#'   , verbose=FALSE
#' )
#' }
#' @export
miceRanger <- function(
    data
  , m = 5
  , maxiter = 5
  , vars
  , valueSelector = c("meanMatch","value")
  , meanMatchCandidates = pmax(round(nrow(data)*0.01),5)
  , returnModels = FALSE
  , parallel = FALSE
  , verbose = TRUE
  , ...
)
{
  
  # Define Variables.
  if (missing(vars)) {
    vars <- sapply(names(data),function(x) setdiff(names(data),x),USE.NAMES = TRUE,simplify = FALSE)
  } else {
    vars <- defineVars(vars,names(data))
  }
  
  # 4 Objects are tracked which pertain to the variables
      # vars - A list. Names are variables to impute, elements are vectors of predictors. 
      # varn - Character vector of variables to impute
      # varp - Character vector of all predictor variables
      # vara - Character vector of all variables which will be used in any context.
  varn <- names(vars)
  varp <- unique(unlist(vars))
  vara <- unique(c(varn,varp))
  
  # Create a copy of dataset so original is not modified.
  # This dataset is used to assign by reference throughout the process.
  # Fastest method, at the expense of duplicating the dataset.
  dat <- copy(data)
  setDT(dat)
  
  # Missing Value Fidelity.
  # Re-define variable setup if needed.
  naWhere <- is.na(dat)
  if (any(apply(naWhere,2,sum) == nrow(dat))) stop("At least 1 column in data contains all missing values.")
  missingCounts <- apply(dat[,varn,with=FALSE],MARGIN = 2,function(x) sum(is.na(x)))
  if (all(missingCounts == 0)) stop("There are no missing values in this dataset.")
  takeOut <- missingCounts == 0 & names(missingCounts) %in% varn
  if (sum(takeOut) > 0) {
    if(verbose) {
      message(
          "One or more of the specified variables to impute contains no missing values. "
        , "These will remain as a predictor, however they will not be imputed. "
      )
    }
    vars <- vars[!takeOut]
    varn <- names(vars)
    varp <- unique(unlist(vars))
    vara <- unique(c(varn,varp))
  }
  
  # Define Value Selector
  valueSelector <- defineValueSelector(valueSelector,vars)
  
  # These variables will be filled in randomly, and then remain 
  # unchanged throughout the process. This is usually not a good idea.
  leftOut <- !varp %in% varn & colSums(naWhere[,varp]) > 0
  if (any(leftOut) & verbose) {
    leftOut <- names(leftOut[leftOut])
    message(
      paste0(
        paste0(leftOut,collapse = ",")
        ," are designated as predictors, but will not be imputed even though they contain missing values."
        ," This may cause imputations to be biased, or otherwise non-optimal."
      )
    )
  }
  
  # These characters in variable names cause problems when plotting:
  checkSpecChars(vara)
  
  # Only keep columns that can be used.
  if (any(!names(dat) %in% vara)) dat[,(setdiff(names(dat),vara)) := NULL]

  # Keep track of datatypes
  rawClasses <- sapply(dat[,vara,with=FALSE],class)
  toFactr <- names(rawClasses[rawClasses=="character"])
  
  # Convert character columns to factors.
  if (any(rawClasses == "character")) {
    if(verbose) message("Converting characters to factors.")
    dat[,(toFactr) := lapply(.SD,factor),.SDcols=toFactr]
  }
  
  # Convert any integer variables to double if we aren't mean matching.
  # If we don't, data.table will complain when we try to complete the data.
  # Numeric stored with 0 precision is fine.
  intToDouble <- rawClasses[varn] == "integer" & valueSelector[varn] == "value"
  if (any(intToDouble)) {
    if(verbose) message("valueSelector == 'value', so interpolation is possible. Converting integers to doubles so precision isn't lost.")
    intToDouble <- names(intToDouble[intToDouble])
    dat[,(intToDouble) := lapply(.SD,as.double),.SDcols=intToDouble]
  }
  
  # Fill missing data with random samples from the nonmissing data.
  for (v in vara) set(dat,i=which(naWhere[,v]),j=v,value=fillMissing(sum(naWhere[,v]),dat[[v]]))
  
  # Keep track of our new classes and the type of model.
  newClasses <- sapply(dat[,vara,with=FALSE],class)
  modelTypes <- ifelse(newClasses[varn] == "factor","Classification","Regression")

  # Define meanMatchCandidates
  meanMatchCandidates <- defineMMC(meanMatchCandidates,modelTypes,valueSelector,naWhere)
  
  startTime <- Sys.time()
  if (verbose) cat("\nProcess started at",as.character(startTime),"\n")
  # Begin Iteration.
  datSetList <- runIterations(
      dat
    , m
    , maxiter
    , vars
    , naWhere
    , valueSelector
    , meanMatchCandidates
    , modelTypes
    , verbose
    , parallel
    , miceObj = NULL
    , oldm = 0
    , oldIt = 0
    , startTime
    , returnModels
    , ...
  )
  endTime <- Sys.time()
  
  # Foreach won't combine into a list if m = 1.
  # This keeps the extraction below much more simple.
  if (m == 1) datSetList <- list(Dataset_1 = datSetList)
  
  # See if foreach returned any errors.
  errorIndx <- sapply(datSetList,function(x) any(class(x) %in% c("simpleError","error")))
  if (any(errorIndx)) {
    stop(
      paste0(
        "Evaluation failed with error <",as.character(datSetList[errorIndx][[1]])
        ,">. This is probably our fault - please open an issue at https://github.com/FarrellDay/miceRanger/issues"
        ," with a reproduceable example."
      )
    )
  }
  
  # Extract parts we are interested in from returned list.
  allImps <- lapply(datSetList,function(x) x$dsImps)
  allImport <- lapply(datSetList,function(x) x$dsImport)
  allError <- lapply(datSetList,function(x) x$dsError)
  finalModels <- lapply(datSetList,function(x) x$dsMod)
  rm(datSetList)
  
  # And finally, adjust names.
  names(allImps) <- paste0("Dataset_",1:m)
  names(allImport) <- paste0("Dataset_",1:m)
  names(allError) <- paste0("Dataset_",1:m)
  names(finalModels) <- paste0("Dataset_",1:m)
  
  # Take necessary info from last iteration.
  finalImps <- lapply(allImps,function(x) x[[length(x)]])
  finalImport <- lapply(allImport,function(x) x[[length(x)]])
  finalError <- rbindlist(lapply(allError,function(x) tail(x,1)))
  setnames(finalError,"iteration","dataset")
  finalError$dataset <- 1:m
  
  
  miceDefs <- list()
  miceDefs$callParams <- list()
  miceDefs$callParams$m <- m
  miceDefs$callParams$maxiter <- maxiter
  miceDefs$callParams$vars <- vars
  miceDefs$callParams$valueSelector <- valueSelector
  miceDefs$callParams$meanMatchCandidates <- meanMatchCandidates
  miceDefs$callParams$returnModels <- returnModels
  miceDefs$data <- data.table(data)
  miceDefs$naWhere <- naWhere
  miceDefs$missingCounts <- missingCounts
  miceDefs$rawClasses <- rawClasses
  miceDefs$newClasses <- newClasses
  miceDefs$allImps <- allImps
  miceDefs$allImport <- allImport
  miceDefs$allError <- allError
  miceDefs$finalImps <- finalImps
  miceDefs$finalImport <- finalImport
  miceDefs$finalError <- finalError
  if (returnModels) miceDefs$finalModels <- finalModels
  miceDefs$imputationTime <- round(as.numeric(difftime(endTime,startTime,units="secs")))
  
  class(miceDefs) <- "miceDefs"
  
  return(miceDefs)
  
}

