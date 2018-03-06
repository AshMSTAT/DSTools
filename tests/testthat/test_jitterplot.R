context("Testing JitterPlot")


test_that("Error messages are not working properly",{
  testthat::expect_error(jitter_plot(iris, "Species", "setosa", "Petal.Width","Sepal.Width", ylimit = 0)
                         , "The y limit must be greater than 0")


})

test_that("Error messages are not working properly",{
  testthat::expect_error(jitter_plot(iris, "Species", "setosa", "Petal.Width","Sepal.Width", ylimit = 10)
                         , "The ylimit can not be larger than the max value of the data")


})


test_that("Error messages are not working properly",{
  testthat::expect_error(jitter_plot(iris, "Species", "setosa", "Petal.Width","")
                         , "The y variable can not be left blank")


})


test_that("Error messages are not working properly",{
  testthat::expect_error(jitter_plot(iris, "Species", "setosa", "", "Petal.Width")
                         , "The x variable can not be left blank")


})
