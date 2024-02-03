#' @title Run ACMCalculator
#'
#' @description Runs the ACMCalculator shiny application, a GUI for the
#' WHO WPRO online-calculator for excess deaths in countries
#'
#' @keywords graphs datagen models
#' @concept demography GUI shiny epidemiology
#' @export
#' @examples
#' \dontrun{
#' run()
#' }
#'
#'
run <- function() {
  shiny::runApp(system.file("shiny/ACMCalculator", package = "ACMCalculator"), port=7990)
}
