#' Launch the tiler tool
#'
#' @export
run_tiler <- function() {
  appDir <- system.file("tiler", package = "tiler")
  if (appDir == "") {
    stop("Unable to find App. Try re-installing the `tiler` package.", call. = FALSE)
  }

  shiny::runApp(appDir, port = 7000, launch.browser = TRUE, display.mode = "normal")
}
