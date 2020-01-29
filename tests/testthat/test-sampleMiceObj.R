context('Running miceRanger')

set.seed(1991)
data(iris)
ampIris <- amputeData(iris)
miceObj <- miceRanger(
    ampIris
  , m=3
  , maxiter=3
  , vars=list(
    Petal.Width = c("Sepal.Length","Sepal.Width","Petal.Length","Species")
    , Species = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
   )
  , verbose = FALSE
)

moComp <- completeData(miceObj)

expect_true(all(sapply(moComp,nrow) == 150))
expect_true(
  all(
    sapply(
        moComp
      , function(x) {
        all(stats::complete.cases(x[,c("Petal.Width","Species")]))
      }
    )
  )
)