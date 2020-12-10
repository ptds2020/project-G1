#' @title Body Mass Index (BMI)
#' @description Return the value of the body mass index (BMI)
#'
#' BMI = Weight/Size^2
#' @param x Your size in centimeters.
#' @param y Your weight in kilograms.
#' @export
#' @examples bmi(x=183, y=75)

bmi <- function(x, y) {
  bmi_final <- (y / (x * x)) * 10000
  bmi_final
}




