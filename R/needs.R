#' @title Calorie need
#' @description Return the daily calorie need for a person.
#' @param bmr The value of your basal metabolic rate (bmr).
#' @param multiplier Multiplier depending on your weekly physical activities (Very light = 1.30, Light = 1.55, Moderate = 1.65, Heavy = 1.80, Very heavy = 2).
#' @export
#' @import magrittr dplyr scales stringr kableExtra ggplot2 stats ggforce
#' @examples
# needs(bmr=1620, multiplier=1.65)

needs <- function(bmr, multiplier) {
  needs_final <- (bmr * multiplier)
  needs_final
}