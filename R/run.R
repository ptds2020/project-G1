#' @title Run the McDonald shiny app
#' @author Gaëtan Lovey, Cédric Vuignier, Jérémy Bayer, Raffaello Raffin
#' @export

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


