library(usethis)
library(devtools)
library(roxygen2)


calculate_cooks_distance <- function(model, data) {
  validate_inputs(data, model)

  residuals<-model$residuals
  hat_values<-hatvalues(model)

  n<-length(residuals)
  p<-length(model$coefficients)

  mse<-sum(residuals^2)/(n-p)
  cooks_d<- (residuals^2 /(p*mse))*(hat_values/(1-hat_values)^2)
  return(cooks_d)
}

calculate_dffits <- function(model, data) {
  validate_inputs(data, model)

  # Calculate leverage values
  h <- hatvalues(model)

  # Calculate residuals
  e <- residuals(model)

  # Standard error of residuals
  s <- summary(model)$sigma

  # Calculate DFFITS
  dffits <- (e / (s * sqrt(1 - h))) * sqrt(h / (1 - h))

  return(dffits)
}



calculate_hadis_influence <- function(model, data) {
  validate_inputs(data, model)

  # Calculate leverage values
  h <- hatvalues(model)

  # Calculate studentized residuals
  r <- rstudent(model)

  # Calculate Hadi's Influence Measure
  hadi <- h * (r^2 / (1 - h))

  return(hadi)
}




validate_inputs<-function(data,model){
  if (!inherits(model, "lm")) {
    stop("The model must be an object of class 'lm'.")
  }

  if (!is.data.frame(data)) {
    stop("The data must be a data frame.")
  }

  if (any(is.na(data))) {
    stop("NA values found in the data.")
  }

  infinite_check <- sapply(data, function(x) any(is.infinite(x)))
  if (any(infinite_check)) {
    stop("Infinite values found in the data.")
  }

  if (nrow(data) != length(residuals(model))) {
    stop("Mismatch between the number of rows in the data and the number of residuals in the model.")
  }
}





plot_influence_measures <- function(model, data, method = "cooks") {
  # Define valid methods
  valid_methods <- c("cooks", "dffits", "hadi")

  # Check if the method is valid
  if (!method %in% valid_methods) {
    stop("Invalid method. Choose from 'cooks', 'dffits', or 'hadi'.")
  }

  validate_inputs(data, model)

  # Calculate influence measures based on the selected method
  influence_values <- switch(method,
                             cooks = calculate_cooks_distance(model, data),
                             dffits = calculate_dffits(model, data),
                             hadi = calculate_hadis_influence(model, data))

  # Determine the threshold based on the method
  threshold <- switch(method,
                      cooks = 4 / nrow(data),  # Common threshold for Cook's Distance
                      dffits = 2 * sqrt(length(coef(model))) / sqrt(nrow(data)),  # DFFITS threshold
                      hadi = NA)  # No standard threshold for Hadi's measure

  # Plot the influence values
  plot(influence_values, type = "h", main = paste(method, "Influence Measures"),
       ylab = paste(method, "Value"), xlab = "Observation Index")

  # Add the threshold line if applicable
  if (!is.na(threshold)) {
    abline(h = threshold, col = "red", lty = 2)  # Add a horizontal red dashed line at the threshold
    text(x = 1, y = threshold, labels = paste("Threshold =", round(threshold, 4)), pos = 4, col = "red")
  }

  # Identify and label points above the threshold
  if (!is.na(threshold)) {
    points_above_threshold <- which(influence_values > threshold)
    if (length(points_above_threshold) > 0) {
      points(points_above_threshold, influence_values[points_above_threshold], col = "blue", pch = 19)

      # Add a slight jitter to the labels to prevent overlap
      text(points_above_threshold, influence_values[points_above_threshold] + 0.002,
           labels = points_above_threshold, pos = 1, col = "blue")
    }
  }

  return(invisible(influence_values))
}


# Fit the model
model <- lm(mpg ~ wt + hp, data = mtcars)

# Plot Cook's Distance using the full method name
plot_influence_measures(model, mtcars, method = "cooks")

# Plot DFFITS
plot_influence_measures(model, mtcars, method = "dffits")

# Plot Hadi's Influence Measure
plot_influence_measures(model, mtcars, method = "hadi")

# This will throw an error since abbreviations are no longer allowed
#plot_influence_measures(model, mtcars, method = "c")


library(testthat)
#use_testthat()

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

  # Test with an abbreviation (which should now be invalid)
  expect_error(plot_influence_measures(model, mtcars, method = "c"),
               "Invalid method. Choose from 'cooks', 'dffits', or 'hadi'.")
})


