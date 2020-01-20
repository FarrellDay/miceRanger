context('Adding Datasets')

miceObj <- addDatasets(sampleMiceDefs,2,verbose=FALSE)

expect_true(miceObj$callParams$m == 5)