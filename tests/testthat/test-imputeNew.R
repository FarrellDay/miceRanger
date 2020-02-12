context('Imputing New Data')

test_that(
  "Imputing New Data"
  
  , {
    skip_on_cran()
    ampIris <- amputeData(iris)
    
    miceObj <- miceRanger(
      ampIris
      , m = 3
      , maxiter = 3
      , returnModels = TRUE
      , verbose = FALSE
    )
    
    miceObjV <- miceRanger(
      ampIris
      , m = 3
      , maxiter = 3
      , returnModels = TRUE
      , valueSelector = "value"
      , verbose = FALSE
    )
    
    newDat <- amputeData(iris)
    newImp <- impute(newDat,miceObj,verbose = FALSE)
    newImpV <- impute(newDat,miceObjV,verbose=FALSE)
    
    expect_true(all(sapply(newImp$imputedData,nrow) == 150))
    expect_true(all(sapply(newImpV$imputedData,nrow) == 150))
    expect_true(all(sapply(newImpV$imputedData, function(x) {all(stats::complete.cases(x))})))
    expect_true(all(sapply(newImpV$imputedData, function(x) {all(stats::complete.cases(x))})))
  }
  
)
