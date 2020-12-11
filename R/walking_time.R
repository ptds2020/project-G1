#' @title Walking time
#' @author Gaëtan Lovey, Cédric Vuignier, Jérémy Bayer, Raffaello Raffin
#' @description Returns the walking time in minutes useful for expending the calories absorbed.
#'
#' Walking time = Kcal/((MET*3.5*Weight)/200)
#' @param y Your weight in kilograms.
#' @param MET Metabolic Equivalent of Task (normal walking = 3, fast walking = 4.5).
#' @param e Fix value of 3.5.
#' @param f Fix value of 200.
#' @param kcal Calorie intake.
#' @export
#' @examples walking_time(y=75, MET = 3, e = 3.5, f = 200, kcal = 800)

walking_time <- function(y, MET, e, f, kcal) {
    walking_time <- (kcal/((MET*e*y)/f))
    walking_time
}


