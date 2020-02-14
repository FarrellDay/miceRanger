context('Adding Iterations')

test_that(
  "Adding Iterations"
  
  , {
    skip_on_cran()
    miceObj <- addIterations(sampleMiceDefs,2,verbose=FALSE)
    compDat <- completeData(miceObj)
    expect_true(miceObj$callParams$maxiter == 5)
  }
  
)
