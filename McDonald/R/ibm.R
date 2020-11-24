
#' @export
#' @import magrittr dplyr scales stringr kableExtra ggplot2 stats ggforce

ibm <- function(x, y) {
  ibm_final <- (y / (x * x)) * 10000
  ibm_final
}

# ibm <- ibm(x, y)


