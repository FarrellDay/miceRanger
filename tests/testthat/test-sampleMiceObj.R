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

expect_equal(miceObj,sampleMiceDefs)