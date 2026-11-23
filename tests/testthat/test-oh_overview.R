# ============================================================
# OneHealthVizR
# File: test-oh_overview.R
# Purpose: Unit tests for oh_overview()
# ============================================================


test_that("oh_overview returns a valid overview object", {

  study <- create_valid_study()

  result <- oh_overview(study)

  expect_s3_class(result, "OneHealthOverview")

  expect_equal(result$study_id, "STUDY001")

  expect_true(is.data.frame(result$table_summary))

  expect_true(is.data.frame(result$identifier_summary))

})


test_that("oh_overview summarizes available tables", {

  study <- create_valid_study()

  result <- oh_overview(study)

  expect_equal(
    result$table_summary$table,
    "human"
  )

  expect_equal(
    result$table_summary$records,
    1L
  )

  expect_equal(
    result$table_summary$variables,
    ncol(study$human)
  )

})


test_that("oh_overview counts missing values", {

  study <- create_valid_study()

  study$human$age <- NA_character_
  study$human$sex <- ""

  result <- oh_overview(study)

  expect_equal(
    result$table_summary$missing_values,
    2L
  )

})


test_that("oh_overview counts distinct identifiers", {

  study <- create_valid_study()

  result <- oh_overview(study)

  sample_summary <- result$identifier_summary[
    result$identifier_summary$identifier == "sample_id",
    ,
    drop = FALSE
  ]

  expect_equal(
    sample_summary$distinct_values,
    1L
  )

})


test_that("oh_overview handles partial studies", {

  study <- create_valid_study()

  # Preserve the required object components with NULL values.

  study["animal"] <- list(NULL)
  study["environment"] <- list(NULL)
  study["microbial"] <- list(NULL)

  result <- oh_overview(study)

  expect_equal(
    result$table_summary$table,
    "human"
  )

  expect_equal(
    result$table_summary$records,
    1L
  )

})


test_that("oh_overview does not modify original data", {

  study <- create_valid_study()

  original_human <- study$human

  oh_overview(study)

  expect_identical(
    study$human,
    original_human
  )

})


test_that("oh_overview rejects invalid objects", {

  expect_error(
    oh_overview(list(study_id = "STUDY001"))
  )

})


test_that("print.OneHealthOverview prints the overview", {

  study <- create_valid_study()

  result <- oh_overview(study)

  expect_output(
    print(result),
    "Study overview"
  )

})
