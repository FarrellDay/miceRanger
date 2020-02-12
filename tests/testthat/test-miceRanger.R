test_that(
    "miceRanger - complex specifications"
    
  , {
    skip_on_cran()
    data(iris)
    ampDat <- amputeData(iris)
    
    v <- list(
      Sepal.Width = c("Sepal.Length","Petal.Width","Species")
      , Sepal.Length = c("Sepal.Width","Petal.Width")
      , Species = c("Sepal.Width")
    )
    pmm <- c(
      Sepal.Width = "meanMatch"
      , Sepal.Length = "value"
      , Species = "meanMatch"
    )
    
    miceObj <- miceRanger(
      ampDat
      , vars = v
      , valueSelector = pmm
      , verbose=FALSE
    )
  }
  
)
