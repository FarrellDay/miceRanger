context('Adding Iterations')

miceObj <- addIterations(sampleMiceDefs,2,verbose=FALSE)

expect_true(miceObj$callParams$maxiter == 5)