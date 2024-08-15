# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

test_that("calculate_cooks_distance works correctly", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  cooks_d <- calculate_cooks_distance(model, mtcars)
  expect_type(cooks_d, "double")
  expect_equal(length(cooks_d), nrow(mtcars))
})

test_that("validate_inputs catches incorrect inputs", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)

  expect_silent(validate_inputs(mtcars, model))

  mtcars_na <- mtcars
  mtcars_na$wt[1] <- NA
  expect_error(validate_inputs(mtcars_na, model), "NA values found in the data.")

  mtcars_inf <- mtcars
  mtcars_inf$wt[1] <- Inf
  expect_error(validate_inputs(mtcars_inf, model), "Infinite values found in the data.")

  expect_error(validate_inputs(as.matrix(mtcars), model), "The data must be a data frame.")

  mtcars_sub <- mtcars[-1, ]
  expect_error(validate_inputs(mtcars_sub, model), "Mismatch between the number of rows in the data and the number of residuals in the model.")
})




test_that("plot_influence_measures handles valid method arguments", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)

  # Test with full valid method names
  expect_silent(plot_influence_measures(model, mtcars, method = "cooks"))
  expect_silent(plot_influence_measures(model, mtcars, method = "dffits"))
  expect_silent(plot_influence_measures(model, mtcars, method = "hadi"))
})

test_that("plot_influence_measures throws error for invalid method arguments", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)

  # Test with an invalid method name
  expect_error(plot_influence_measures(model, mtcars, method = "invalid"),
               "Invalid method. Choose from 'cooks', 'dffits', or 'hadi'.")

  # Test with an abbreviation
  expect_error(plot_influence_measures(model, mtcars, method = "c"),
               "Invalid method. Choose from 'cooks', 'dffits', or 'hadi'.")
})



