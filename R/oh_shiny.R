# ============================================================
# OneHealthVizR
# File: R/oh_shiny.R
# Purpose: Launch the OneHealthVizR Shiny application
# ============================================================

#' Launch the OneHealthVizR Shiny Application
#'
#' Opens the interactive Shiny interface for importing,
#' validating, summarizing and visualizing One Health data.
#'
#' @param launch.browser Logical. If TRUE, opens the application
#'   in the default web browser.
#' @param ... Additional arguments passed to shiny::runApp().
#'
#' @return The result returned by shiny::runApp().
#'
#' @importFrom utils packageVersion
#' @export
#'
#' @examples
#' \dontrun{
#' run_onehealthvizr()
#' }
run_onehealthvizr <- function(
    launch.browser = interactive(),
    ...
) {

  # Check Shiny dependency
  if (!requireNamespace("shiny", quietly = TRUE)) {

    stop(
      "The 'shiny' package is required. ",
      "Install it using install.packages('shiny').",
      call. = FALSE
    )
  }

  # Locate installed application
  app_dir <- system.file(
    "shiny",
    package = "OneHealthVizR"
  )

  # Check application directory
  if (
    !nzchar(app_dir) ||
    !dir.exists(app_dir) ||
    !file.exists(file.path(app_dir, "app.R"))
  ) {

    stop(
      "The OneHealthVizR Shiny application was not found. ",
      "Reinstall the package and try again.",
      call. = FALSE
    )
  }

  # Launch application
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    ...
  )
}
