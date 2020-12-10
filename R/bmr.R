#' @title Basal Metobolic Rate (BMR)
#' @description Return the value of the basal metabolic rate (BMR).
#'
#' BMR = Weight*Coefficient*24*LeanFactor
#' @param y Your weight in kilograms.
#' @param d Fix value of 24.
#' @param coefficient Coefficent that corresponds to the gender: 1 for a male and 0.9 for a female.
#' @param leanfactor Lean factor that depends on the gender and the bmi value (between 0.85 and 1).
#' @export
#' @examples bmr(y=75, coefficient = 1, d = 24, leanfactor = 0.9)

bmr <- function(y, coefficient, d, leanfactor ) {
  bmr_final <- (y * coefficient * d * leanfactor)
  bmr_final
}


