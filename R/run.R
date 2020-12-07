#' @title Run the McDonald shiny app
#' @export

run <- function() {
  appDir <- system.file("shiny-examples", "McDonald", package = "McDonald")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing MacDonald.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal", quiet = TRUE)

}


