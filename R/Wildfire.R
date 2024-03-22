#' @name Wildfire
#' @title Wild Fire Risk Assessment in Santa Barbara
#' @description
#'
#'  This package includes functions to calculate the risk score for wild fires in
#'  Santa Barbara using environmental parameters such as temperature, precipitation,
#'  and humidity, and to classify this score into risk levels.
#'
#' The main functions are:
#' \itemize{
#'   \item{\code{risk_score}}{: Calculates a numerical risk score based on input parameters}
#'   \item{\code{risk_level}}{: Classifies the calculated risk score into a risk level category}
#' }
#'
#' @usage
#' risk_score(T, p, h) risk_level(risk_score)
#'
#' @param T Numeric, annual mean temperature in Celsius.
#' @param p Numeric, annual mean precipitation in mm.
#' @param h Numeric, annual mean humidity in percent (%).
#' @return The \code{risk_score} function returns a numeric risk score based on the input parameters.
#'
#' The \code{risk_level} function returns a character string indicating the risk level:
#' "no risk", "low risk", "medium risk", or "high risk"
#'

#' @author Author:
#' Livia Costa and Zoe Zhou

#' @examples
#' Example:
#' risk_score(25, 200, 45)
#' risk_level(320)
#'
#' @export


  risk_score = function(T, p, h) {
    risk_score = 0.5*T**2 + 0.2*(400-p)+0.3*(100-h)

    # error check: make sure units are correct, celsius for temperature, mm for precipitation, % for humidity
    if (p < 0) stop("Mean Precipitation (p) cannot be negative")
    if (T < 0) stop("Mean Temperature (T) cannot be negative")
    if (p > 400) warning("extreme high precipitation")

    return(risk_score)
  }

  risk_level = function(risk_score) {
    if (risk_score < 291.9) {
      return("no risk")
    } else if (risk_score < 327.8) {
      return("low risk")
    } else if (risk_score >= 327.8 && risk_score <= 514.5) {
      return("medium risk")
    } else if (risk_score > 514.5) {
      return("high risk")
    }
  }


