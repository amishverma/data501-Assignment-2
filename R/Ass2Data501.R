#' Calculate Cook's Distance
#'
#' This function calculates Cook's Distance for each observation in a linear model.
#'
#' @param model A linear model object of class \code{lm}.
#' @param data A data frame containing the data used to fit the linear model.
#' The data should have the same structure and number of rows as the model's data
#'
#' @return A numeric vector of Cook's Distance values for each observation in the model.
#' @export
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' cooks_d <- calculate_cooks_distance(model, mtcars)
#' plot_influence_measures(model,mtcars,method="cooks")
#'
#'
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









#' Calculate DFFITS
#' This function calculates the DFFITS value for each observation in a linear model.
#' DFFITS is a measure used to assess the influence of each observation on the fitted values of a regression model.
#'
#' @param model A linear model object of class \code{lm}.
#' @param data A data frame containing the data used to fit the linear model.
#' The data should have the same structure and number of rows as the model's data
#'
#' @return A numeric vector of DFFITS values for each observation in the model
#' @export
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' dffits_values <- calculate_dffits(model, mtcars)
#'
#'
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



#' Calculate Hadi's Influence Measure.
#'
#' This function calculates Hadi influence measure for each observation in a linear model.
#' Hadi's influence measure is used to identify influential observations that have an impact on the regression model.
#'
#' @param model A linear model object of class \code{lm}
#' @param data  A data frame containing the data used to fit the linear model.
#'
#' @return A numeric vector of Hadi's influence measure values for each observation in the model.
#' @export
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' hadi_values <- calculate_hadis_influence(model, mtcars)
#'
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




#' Validate Inputs for Influence Measure Calculations
#'
#' This function validates the inputs to ensure they are suitable for calculating influence measures.
#' @param data A data frame containing the data used to fit the linear model.
#' @param model A linear model object of class \code{lm}.
#'
#' @return  No return value since the function is used for checking
#' @export
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' validate_inputs(mtcars, model)
#'
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





#' Plot Influence Measures for Linear Models
#'
#' This function plots the influence measures (Cook's Distance, DFFITS, or Hadi's Influence Measure)
#'
#' @param model A linear model object of class \code{lm}.
#' @param data  A data frame containing the data used to fit the linear model.
#' @param method A character string specifying which influence measure to plot.
#' Must be one of \code{"cooks"}, \code{"dffits"}, or \code{"hadi"}.
#'
#' @return A plot of the selected influence measure with points above the threshold labeled.
#' @export
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' plot_influence_measuress(model,mtcars,method="cooks")
#'
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






