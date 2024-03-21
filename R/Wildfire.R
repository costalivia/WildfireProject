#' @title Wild fire risk assessment in Santa Barbara
#' @description
#' This function provides wild fire risk level as a function of temperature, precipitation, humidity,
#' @author Livia Costa and Zoe Zhou
#' @param  T annual mean temperature  (Celsius)
#' @param p annual mean precipitation  (mm)
#' @param h annual mean humidity in (%)
#' @returns risk_score
#' @returns risk_level: low, medium, high

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

