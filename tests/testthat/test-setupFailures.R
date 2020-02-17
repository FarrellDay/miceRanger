test_that(
  
  "miceRanger - Setup Failures"
  
  , {
    
    skip_on_cran()
    data(iris)
    ampDat <- amputeData(iris)

    # vars isn't a column
    expect_error(
      {
        miceRanger(
          data = ampDat
          , vars = c("NonExistantColumn")
        )
      }
      , "At least 1 entry in vars is not a column in data."
    )
    
    # Predictor isn't a column
    expect_error(
      {
        miceRanger(
          data = ampDat
          , vars = list("Sepal.Width" = "NonExistantColumn")
        )
      }
      , "At least 1 predictor provided in vars is not a column in data."
    )
    
    # No Missing Values
    expect_error(
      {
        miceRanger(
          data = iris
        )
      }
      , "There are no missing values in this dataset."
    )
    
    # No missing values for specified variables
    expect_error(
      {
        miceRanger(
          data = amputeData(iris,cols="Sepal.Width")
          , vars = "Petal.Width"
        )
      }
      , "There are no missing values in this dataset."
    )
    
    # Bad valueSelector
    expect_error(
      {
        miceRanger(
          data = ampDat
          , valueSelector = "Huh"
        )
      }
      , "valueSelector not recognized"
    )
    
    
    expect_error(
      {
        miceRanger(
          data = ampDat
          , vars = "Sepal.Width"
          , valueSelector = c("Petal.Width" = "value")
        )
      }
      , "Names in valueSelector do not match variables to impute specified by vars."
    )

  }

)
