# ============================================================
# OneHealthVizR
# File: test-oh_validate.R
# Purpose: Unit tests for oh_validate()
# ============================================================


# Helper: create a valid OneHealthStudy object
create_valid_study <- function() {

  human_data <- data.frame(
    study_id = "STUDY001",
    host_id = "H001",
    site_id = "SITE001",
    event_id = "EVENT001",
    sample_id = "SAMPLE001",
    age = "25",
    sex = "Female",
    occupation = "Farmer",
    sample_type = "Blood",
    collection_date = "2026-05-12",
    infection_status = "Positive"
  )

  file_path <- tempfile(fileext = ".csv")

  utils::write.csv(
    human_data,
    file = file_path,
    row.names = FALSE
  )

  study <- oh_import(human = file_path)

  unlink(file_path)

  study
}


test_that("oh_validate accepts a valid OneHealthStudy object", {

  study <- create_valid_study()

  expect_invisible(
    oh_validate(study)
  )

})


test_that("oh_validate rejects an object with an invalid class", {

  expect_error(
    oh_validate(list(study_id = "STUDY001")),
    "must inherit from 'OneHealthStudy'"
  )

})


test_that("oh_validate rejects an object missing required components", {

  study <- create_valid_study()

  study$human <- NULL

  names(study) <- setdiff(names(study), "human")

  expect_error(
    oh_validate(study),
    "missing components"
  )

})


test_that("oh_validate rejects inconsistent study identifiers", {

  study <- create_valid_study()

  study$human$study_id <- "STUDY002"

  expect_error(
    oh_validate(study),
    "inconsistent with the study object"
  )

})


test_that("oh_validate rejects missing primary keys", {

  study <- create_valid_study()

  study$human$sample_id <- ""

  expect_error(
    oh_validate(study),
    "missing values in 'sample_id'"
  )

})


test_that("oh_validate rejects duplicated primary keys", {

  study <- create_valid_study()

  study$human <- rbind(
    study$human,
    study$human
  )

  study$human$host_id[2] <- "H002"

  expect_error(
    oh_validate(study),
    "duplicated values in 'sample_id'"
  )

})


test_that("oh_validate rejects missing required columns", {

  study <- create_valid_study()

  study$human <- study$human[
    ,
    names(study$human) != "occupation",
    drop = FALSE
  ]

  expect_error(
    oh_validate(study),
    "missing required columns"
  )

})


test_that("oh_validate rejects a non-data-frame table", {

  study <- create_valid_study()

  study$human <- "invalid data"

  expect_error(
    oh_validate(study),
    "must be a data frame"
  )

})
