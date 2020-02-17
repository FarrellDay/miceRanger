# Draws nk nonmissing values randomly from pullFrom
fillMissing <- function(nk,pullFrom) {
  sample(
      pullFrom[!is.na(pullFrom)]
    , size = nk
    , replace=TRUE
  )
}

# Define parallelization method for foreach
ParMethod <- function(x) if(x) {`%dopar%`} else {`%do%`}

# Check for special characters in column names
checkSpecChars <- function(vara) {
  specCharsExist <- sapply(
    c(" ","-","/","=","!","@","%","<",">")
    , function(v) any(lengths(regmatches(vara, gregexpr(v, vara))) > 0)
  )
  if(any(specCharsExist)) {
    message("Some variable names contain characters which will cause parsing issues when plotting: (' ','-','/',...).\nContinue? [y/n]")
    line <- readline()
    if(tolower(line)=="y") invisible() else stop("Process Stopped By User.")
  }
}

# Re-format meanMatchCandidates and check for problems
defineMMC <- function(meanMatchCandidates,modelTypes,valueSelector,naWhere) {
  
  # Collect numeric variables, as well as those which need to be assigned
  # a mean matching candidates number.
  regModelVars <- names(modelTypes[modelTypes == "Regression"])
  needsMeanMatch <- intersect(regModelVars,names(valueSelector[valueSelector == "meanMatch"]))
  
  # Define meanMatchCandidates parameter. If a named vector was provided by the
  # user, check its validity.
  if (is.null(names(meanMatchCandidates)) & length(needsMeanMatch) > 0) {
    
    meanMatchCandidates <- rep(meanMatchCandidates,length(needsMeanMatch))
    names(meanMatchCandidates) <- needsMeanMatch
    
  } else {
    
    if (!all(needsMeanMatch %in% names(meanMatchCandidates))) {
      stop("Names of meanMatchCandidates does not contain all numeric variables scheduled to be imputed with valueSelector = 'meanMatch'.")
    }
    if (!class(meanMatchCandidates) %in% c("integer","numeric")) {
      stop("meanMatchCandidates must be a named vector of integers or a single integer.")
    }
    
    # Only keep mean match candidates for variables that are using mean matching.
    meanMatchCandidates <- meanMatchCandidates[needsMeanMatch]
    
  }
  
  if (any(sapply(needsMeanMatch,function(x) !sum(!is.na(naWhere[,x])) >= meanMatchCandidates[[x]]))) {
    stop("Not enough non-missing values to satisfy meanMatchCandidates in at least one variable.")
  }
  
  return(meanMatchCandidates)
  
}

# Re-format vars
defineVars <- function(vars,datNames) {
  
  if (class(vars) == "character") {
    
    if (any(!vars %in% datNames)) stop("At least 1 entry in vars is not a column in data.")
    vars <- sapply(vars,function(x) setdiff(datNames,x),USE.NAMES = TRUE,simplify = FALSE)
    
  } else if (class(vars) == "list") {
    
    if (is.null(names(vars))) stop("If a list is specified for vars, the list names must be the variables to impute.")
    if (any(sapply(names(vars),function(x) x %in% vars[[x]]))) stop("A variable cannot be used to impute itself, check vars.")
    if (any(!names(vars) %in% datNames)) stop("At least 1 name in vars is not a column in data.")
    varp <- unique(unlist(vars))
    if (any(!varp %in% datNames)) stop("At least 1 predictor provided in vars is not a column in data.")
    
  } else stop("vars not recognized. Please see ?miceRanger for available options for vars.")
  
  return(vars)
  
}

# Re-format valueSelector
defineValueSelector <- function(valueSelector,vars) {
  
  if(is.null(names(valueSelector))) {
    
    if (!valueSelector[[1]] %in% c("meanMatch","value")) stop("valueSelector not recognized")
    valueSelector <- rep(valueSelector[[1]],length(vars))
    names(valueSelector) <- names(vars)
    
  } else {
    
    if(!setequal(names(vars),names(valueSelector))) stop("Names in valueSelector do not match variables to impute specified by vars.")
    if(!any(valueSelector %in% c("meanMatch","value"))) stop("All elements in valueSelector should be either 'meanMatch' or 'value'.")
    
    # Ensure that valueSelector only contains variables to be imputed.
    valueSelector <- valueSelector[names(vars)]
    
  }
  
  return(valueSelector)
  
}
