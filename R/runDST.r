#' @import shiny
library("shiny")

#' @import sqldf
library("sqldf")

#' @import DT
library("DT")

#' @import tabular
library("tabular")

#' Web-interface version of the DST
#'
#' Runs the DST in a web browser using R/Shiny technology
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

runDST <- function() {
  shiny::runApp(appDir = system.file("app", package = "dswapDST"))
}
