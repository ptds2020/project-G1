#' @title Run the McDonald shiny app
#' @export
#' @import shiny dplyr readxl ggplot2 tidyr kableExtra here shinydashboard reshape2 plotly ECharts2Shiny shinyWidgets

run <- function() {
  appDir <- system.file("shiny-examples", "McDonald", package = "McDonald")
  if (appDir == "") {
    stop(
      "Could not find directory. Try re-installing MacDonald.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal", quiet = TRUE)

}


