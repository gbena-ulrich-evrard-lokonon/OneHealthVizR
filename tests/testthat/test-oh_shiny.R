# ============================================================
# OneHealthVizR
# File: test-oh_shiny.R
# Purpose: Unit tests for run_onehealthvizr()
# ============================================================

test_that("run_onehealthvizr is exported", {

  expect_true(
    "run_onehealthvizr" %in%
      getNamespaceExports("OneHealthVizR")
  )

})

test_that("run_onehealthvizr checks the Shiny dependency", {

  expect_true(
    is.function(
    OneHealthVizR::run_onehealthvizr
    )
  )

})

test_that("Shiny application directory exists", {

  app_dir <- system.file(
    "shiny",
    package = "OneHealthVizR"
  )

  expect_true(
    nzchar(app_dir)
  )

  expect_true(
    dir.exists(app_dir)
  )

  expect_true(
    file.exists(
      file.path(app_dir, "app.R")
    )
  )

})

test_that("Shiny application can be loaded", {

  skip_if_not_installed("shiny")

  app_dir <- system.file(
    "shiny",
    package = "OneHealthVizR"
  )

  app <- shiny::shinyAppFile(
    file.path(app_dir, "app.R")
  )

  expect_s3_class(
    app,
    "shiny.appobj"
  )

})

