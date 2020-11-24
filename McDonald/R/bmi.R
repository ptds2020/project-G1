#' @title BMI
#' @description Return the value of the body mass index (bmi).
#' @param x Your size in centimeters.
#' @param y Your weight in kilograms.
#' @export
#' @import magrittr dplyr scales stringr kableExtra ggplot2 stats ggforce
#' @examples
#' bmi(x=183, y=75)

bmi <- function(x, y) {
  bmi_final <- (y / (x * x)) * 10000
  bmi_final
}




