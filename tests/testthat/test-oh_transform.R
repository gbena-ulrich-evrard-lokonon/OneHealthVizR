test_that("column selection preserves requested columns", {

  data <- data.frame(
    sample_id = c("S01", "S02"),
    age = c("25", "30"),
    sex = c("F", "M")
  )

  result <- .oh_select_columns(
    data,
    c("sample_id", "age")
  )

  expect_named(result, c("sample_id", "age"))
  expect_equal(nrow(result), 2L)
})


test_that("column selection preserves all columns when NULL", {

  data <- data.frame(
    sample_id = "S01",
    age = "25"
  )

  expect_equal(.oh_select_columns(data), data)
})


test_that("column selection rejects unknown columns", {

  data <- data.frame(sample_id = "S01")

  expect_error(
    .oh_select_columns(data, "unknown"),
    "Unknown columns"
  )
})


test_that("row filtering selects matching observations", {

  data <- data.frame(
    sample_id = c("S01", "S02", "S03"),
    sex = c("F", "M", "F"),
    age = c(20, 30, 40)
  )

  result <- .oh_filter_rows(
    data,
    list(sex = "F")
  )

  expect_equal(nrow(result), 2L)
  expect_equal(result$sample_id, c("S01", "S03"))
})


test_that("multiple filters are combined", {

  data <- data.frame(
    sample_id = c("S01", "S02", "S03"),
    sex = c("F", "M", "F"),
    age = c(20, 30, 40)
  )

  result <- .oh_filter_rows(
    data,
    list(
      sex = "F",
      age = c(40)
    )
  )

  expect_equal(result$sample_id, "S03")
})


test_that("row filtering preserves missing values correctly", {

  data <- data.frame(
    sample_id = c("S01", "S02", "S03"),
    sex = c("F", NA, "M")
  )

  result <- .oh_filter_rows(
    data,
    list(sex = c("F", "M"))
  )

  expect_equal(result$sample_id, c("S01", "S03"))
})


test_that("row filtering rejects unknown columns", {

  data <- data.frame(sample_id = "S01")

  expect_error(
    .oh_filter_rows(data, list(unknown = "value")),
    "Unknown filter columns"
  )
})


test_that("row filtering rejects unnamed filters", {

  data <- data.frame(sample_id = "S01")

  expect_error(
    .oh_filter_rows(data, list("S01")),
    "unique, non-empty column names"
  )
})

test_that("numeric transformations work correctly", {

  data <- data.frame(
    age = c(1, 10, 100),
    count = c(0, 1, 3)
  )

  result <- .oh_transform_numeric(
    data,
    list(age = "log", count = "sqrt")
  )

  expect_equal(result$age, log(data$age))
  expect_equal(result$count, sqrt(data$count))
})


test_that("log1p transformation works correctly", {

  data <- data.frame(count = c(0, 1, 3))

  result <- .oh_transform_numeric(
    data,
    list(count = "log1p")
  )

  expect_equal(result$count, log1p(data$count))
})


test_that("scale transformation standardizes values", {

  data <- data.frame(value = c(1, 2, 3))

  result <- .oh_transform_numeric(
    data,
    list(value = "scale")
  )

  expect_equal(mean(result$value), 0, tolerance = 1e-10)
  expect_equal(sd(result$value), 1, tolerance = 1e-10)
})


test_that("numeric transformations preserve missing values", {

  data <- data.frame(value = c(1, NA, 9))

  result <- .oh_transform_numeric(
    data,
    list(value = "sqrt")
  )

  expect_true(is.na(result$value[2]))
})


test_that("log transformation rejects zero and negative values", {

  data <- data.frame(value = c(0, 2, 4))

  expect_error(
    .oh_transform_numeric(data, list(value = "log")),
    "strictly positive"
  )
})


test_that("numeric transformations reject qualitative columns", {

  data <- data.frame(sex = c("F", "M"))

  expect_error(
    .oh_transform_numeric(data, list(sex = "log")),
    "must be numeric"
  )
})


test_that("numeric transformations reject unsupported methods", {

  data <- data.frame(value = c(1, 2, 3))

  expect_error(
    .oh_transform_numeric(data, list(value = "unknown")),
    "Unsupported transformation"
  )
})

test_that("oh_transform returns a OneHealthTransformed object", {

  study <- create_streptococcus_study()
  merged <- oh_merge(study)

  result <- oh_transform(merged, dataset = "samples")

  expect_s3_class(result, "OneHealthTransformed")
  expect_equal(result$dataset, "samples")
  expect_true(is.data.frame(result$data))
})


test_that("oh_transform applies filtering and selection", {

  study <- create_streptococcus_study()
  merged <- oh_merge(study)

  result <- oh_transform(
    merged,
    dataset = "samples",
    filters = list(compartment = "human"),
    select = c("sample_id", "compartment")
  )

  expect_equal(nrow(result$data), 2L)
  expect_named(result$data, c("sample_id", "compartment"))
})


test_that("oh_transform rejects invalid objects", {

  expect_error(
    oh_transform(list(), dataset = "samples"),
    "OneHealthMerged"
  )
})


test_that("oh_transform rejects unavailable datasets", {

  study <- create_valid_study()
  merged <- oh_merge(study)

  expect_error(
    oh_transform(merged, dataset = "environment"),
    "not available"
  )
})


test_that("oh_transform preserves the original object", {

  study <- create_streptococcus_study()
  merged <- oh_merge(study)

  original <- merged$samples

  invisible(
    oh_transform(
      merged,
      dataset = "samples",
      filters = list(compartment = "human")
    )
  )

  expect_equal(merged$samples, original)
})
