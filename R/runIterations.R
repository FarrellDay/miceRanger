
# Performs mice for specified datasets and iterations.

runIterations <- function(
    dat = NULL
  , m
  , maxiter
  , vars
  , naWhere
  , valueSelector
  , meanMatchCandidates
  , modelTypes
  , verbose
  , ParMethod
  , parallel
  , mco
  , miceObj = NULL
  , oldm = 0
  , oldIt = 0
  , ...
) {
  
  # Define parallelization setup
  ParMethod <- function(x) if(x) {`%dopar%`} else {`%do%`}
  `%op%` <- ParMethod(parallel)
  ds <- crayon::make_style("#4B8E78")
  
  # Run iterations
  dsl <- foreach(
    dataSet = 1:m
    , .options.multicore = mco
    , .combine = list
    , .multicombine = TRUE
    , .inorder = FALSE
    , .errorhandling = 'pass'
    , .packages = c('data.table','ranger','FNN')
    , .verbose = FALSE
  ) %op% {
    
    # Move away from for-loops and towards parLapply.
    dsImport <- list()
    dsError <- list()
    dsImps <- list()
    
    # global binding.
    dataSet <- get("dataSet")
    dats <- if (!is.null(miceObj)) copy(completeData(miceObj,datasets=dataSet)[[1]]) else copy(dat)
    
    if(verbose) cat(ds("\ndataset",dataSet + oldm,"\n"))
    
    for (iter in 1:maxiter) {
      
      iterImps <- list()
      iterError <- list()
      iterImport <- list()
      
      if(verbose) cat("iteration",iter + oldIt,"\t")
      
      for (impVar in vars) {

        if(verbose) cat(" |",impVar)
        missIndx <- naWhere[,impVar]
        returnProb <- modelTypes[impVar] == "Classification" & valueSelector == "meanMatch"
        model <- ranger(
          data = dats[!missIndx]
          , dependent.variable.name = impVar
          , importance = "impurity"
          , probability = returnProb
          , verbose = FALSE
          , ...
        )
        pred <- predict(model,dats)$predictions
        iterImps[[impVar]] <- imputeFromPred(pred,modelTypes[impVar],valueSelector,meanMatchCandidates,dats[!missIndx][,get(impVar)],missIndx)
        dats[missIndx,(impVar) := iterImps[[impVar]]]
        iterImport[[impVar]] <- as.data.table(as.list(model$variable.importance))
        if(modelTypes[impVar] == "Regression") {
          iterError[[impVar]] <- model$r.squared
        } else {
          iterError[[impVar]] <- 1-model$prediction.error
        }
        
      }
      
      # Now that the models have been run for this iteration...
      
      # Add to iteration importance list
      dsImport[[iter]] <- rbindlist(iterImport,fill = TRUE)
      dsImport[[iter]]$variable <- vars
      setcolorder(dsImport[[iter]],c("variable",vars))
      
      # Add to iteration model error list
      dsError[[iter]] <- iterError
      
      # Add to iteration imputation list
      dsImps[[iter]] <- iterImps
      
      rm(iterImps,iterError,iterImport)
      
      if(verbose) cat("\n")
      
    }
    
    # Adjust names
    names(dsImps) <- paste0("Iteration_",1:maxiter + oldIt)
    names(dsImport) <- paste0("Iteration_",1:maxiter + oldIt)
    dsError <- rbindlist(dsError)
    dsError$iteration <- 1:maxiter + oldIt
    setcolorder(dsError,c("iteration",vars))
    
    return(
      list(
        dsImport = dsImport
        , dsError = dsError
        , dsImps = dsImps
      )
    )
  }
  return(dsl)
}
