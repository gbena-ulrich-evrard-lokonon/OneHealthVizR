test_that("OneHealthVizR package metadata is correct", {

  description <- utils::packageDescription("OneHealthVizR")

  expect_identical(
    description$Package,
    "OneHealthVizR"
  )

  expect_identical(
    description$Version,
    "0.1.0"
  )

})
