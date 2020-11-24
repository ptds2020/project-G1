
#' @export
#' @import magrittr dplyr scales stringr kableExtra ggplot2 stats ggforce

needs <- function(bmr, multiplier) {
  needs_final <- (bmr * multiplier)
  needs_final
}