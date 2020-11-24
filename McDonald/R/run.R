#' @title Run the McDonald shiny app 
#' @description 
#' @export
#' @import magrittr dplyr scales stringr kableExtra ggplot2 stats ggforce
#'

run <- function() {
  appDir <- system.file("shiny-examples", "McDonald", package = "MacDonald")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing MacDonald.",
      call. = FALSE
    )
  }
  
  shiny::runApp(appDir, display.mode = "normal")
  
}


