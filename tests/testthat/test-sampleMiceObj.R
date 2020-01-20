context('Running miceRanger')

set.seed(1991)
data(iris)
ampIris <- amputeData(iris)
miceObj <- miceRanger(
  ampIris
  ,m=3
  ,maxiter=3
  ,vars=list(
    Petal.Width = c("Sepal.Length","Sepal.Width","Petal.Length","Species")
    , Species = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
  )
)

# Time can differ, that's fine.
impTimIdx <- which(names(miceObj) == "imputationTime")
expect_equal(miceObj[-impTimIdx],sampleMiceDefs[-impTimIdx])

moComp <- completeData(miceObj)
smComp <- completeData(sampleMiceDefs)
expect_equal(moComp,smComp)