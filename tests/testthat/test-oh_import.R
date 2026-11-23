test_that("oh_import imports a valid human table", {

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

  expect_s3_class(study, "OneHealthStudy")
  expect_identical(study$study_id, "STUDY001")
  expect_equal(nrow(study$human), 1L)
  expect_null(study$animal)
  expect_identical(study$human$age, "25")

})


test_that("oh_import rejects a table with missing required columns", {

  human_data <- data.frame(
    study_id = "STUDY001",
    host_id = "H001",
    site_id = "SITE001"
  )

  file_path <- tempfile(fileext = ".csv")

  utils::write.csv(
    human_data,
    file = file_path,
    row.names = FALSE
  )

  expect_error(
    oh_import(human = file_path),
    "missing required columns"
  )

})


test_that("oh_import rejects duplicated sample identifiers", {

  human_data <- data.frame(
    study_id = c("STUDY001", "STUDY001"),
    host_id = c("H001", "H002"),
    site_id = c("SITE001", "SITE001"),
    event_id = c("EVENT001", "EVENT001"),
    sample_id = c("SAMPLE001", "SAMPLE001"),
    age = c("25", "30"),
    sex = c("Female", "Male"),
    occupation = c("Farmer", "Teacher"),
    sample_type = c("Blood", "Blood"),
    collection_date = c("2026-05-12", "2026-05-13"),
    infection_status = c("Positive", "Negative")
  )

  file_path <- tempfile(fileext = ".csv")

  utils::write.csv(
    human_data,
    file = file_path,
    row.names = FALSE
  )

  expect_error(
    oh_import(human = file_path),
    "duplicated values in 'sample_id'"
  )

})


test_that("oh_import requires at least one input file", {

  expect_error(
    oh_import(),
    "At least one input file"
  )

})
