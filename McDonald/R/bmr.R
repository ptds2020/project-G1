#' @title BMR
#' @description Return the value of the basal metabolic rate (bmr).
#' @export
#' @import magrittr dplyr scales stringr kableExtra ggplot2 stats ggforce

bmr <- function(y, coefficient, d, leanfactor) {
  bmr_final <- (y * coefficient * d * leanfactor)
  bmr_final
}