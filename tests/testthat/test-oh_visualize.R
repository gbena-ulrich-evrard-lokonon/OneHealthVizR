# ============================================================
# OneHealthVizR
# File: test-oh_visualize.R
# Purpose: Unit tests for visualization functions
# ============================================================


# Helper: create a valid study for visualization tests
create_visualization_study <- function() {

  human_data <- data.frame(
    study_id = c("STUDY001", "STUDY001"),
    host_id = c("H001", "H002"),
    site_id = c("SITE001", "SITE001"),
    event_id = c("EVENT001", "EVENT001"),
    sample_id = c("SAMPLE001", "SAMPLE002"),
    age = c(25, 30),
    sex = c("F", "M"),
    occupation = c("Farmer", "Teacher"),
    sample_type = c("Blood", "Blood"),
    collection_date = as.Date(c("2026-01-01", "2026-01-02")),
    infection_status = c("Positive", "Negative")
  )

  animal_data <- data.frame(
    study_id = c("STUDY001", "STUDY001"),
    host_id = c("A001", "A002"),
    site_id = c("SITE001", "SITE001"),
    event_id = c("EVENT001", "EVENT001"),
    sample_id = c("SAMPLE003", "SAMPLE004"),
    species = c("Pig", "Chicken"),
    age = c(2, 1),
    sex = c("F", "M"),
    production_type = c("Farm", "Farm"),
    sample_type = c("Swab", "Blood"),
    collection_date = as.Date(c("2026-01-01", "2026-01-02")),
    infection_status = c("Positive", "Negative")
  )

  environment_data <- data.frame(
    measurement_id = c("M001", "M002", "M003", "M004"),
    study_id = rep("STUDY001", 4),
    site_id = rep("SITE001", 4),
    event_id = rep("EVENT001", 4),
    sample_id = rep("", 4),
    parameter = c(
      "Temperature",
      "Temperature",
      "Humidity",
      "pH"
    ),
    value = c("25", "30", "70", "6.5"),
    unit = c("C", "C", "%", "pH"),
    measurement_date = as.Date(rep("2026-01-01", 4)),
    method = rep("Field measurement", 4)
  )

  microbial_data <- data.frame(
    observation_id = c("O001", "O002", "O003", "O004"),
    study_id = rep("STUDY001", 4),
    sample_id = c(
      "SAMPLE001",
      "SAMPLE001",
      "SAMPLE002",
      "SAMPLE003"
    ),
    microorganism_id = c(
      "MICRO001",
      "MICRO002",
      "MICRO001",
      "MICRO003"
    ),
    method = rep("PCR", 4),
    result_type = rep("qualitative", 4),
    result_status = c(
      "positive",
      "negative",
      "inconclusive",
      "not tested"
    ),
    result_value = c("1", "0", NA, NA),
    result_unit = rep(NA_character_, 4)
  )

  structure(
    list(
      study_id = "STUDY001",
      human = human_data,
      animal = animal_data,
      environment = environment_data,
      microbial = microbial_data,
      source_files = NULL
    ),
    class = "OneHealthStudy"
  )
}


# ============================================================
# oh_plot_compartments()
# ============================================================

test_that("oh_plot_compartments returns a ggplot object", {

  study <- create_visualization_study()

  result <- oh_plot_compartments(study)

  expect_s3_class(result, "ggplot")
})


test_that("oh_plot_compartments handles partial studies", {

  study <- create_visualization_study()

  study["animal"] <- list(NULL)
  study["environment"] <- list(NULL)
  study["microbial"] <- list(NULL)

  result <- oh_plot_compartments(study)

  expect_s3_class(result, "ggplot")
})


test_that("oh_plot_compartments rejects studies without available tables", {

  study <- create_visualization_study()

  study["human"] <- list(NULL)
  study["animal"] <- list(NULL)
  study["environment"] <- list(NULL)
  study["microbial"] <- list(NULL)

  expect_error(
    oh_plot_compartments(study),
    "No data are available"
  )
})


# ============================================================
# oh_plot_microbes()
# ============================================================

test_that("oh_plot_microbes returns a ggplot object", {

  study <- create_visualization_study()

  result <- oh_plot_microbes(study)

  expect_s3_class(result, "ggplot")
})


test_that("oh_plot_microbes respects top_n", {

  study <- create_visualization_study()

  result <- oh_plot_microbes(study, top_n = 2)

  expect_s3_class(result, "ggplot")
  expect_equal(nrow(result$data), 2)
})


test_that("oh_plot_microbes rejects invalid top_n", {

  study <- create_visualization_study()

  expect_error(
    oh_plot_microbes(study, top_n = 0),
    "positive integer"
  )

  expect_error(
    oh_plot_microbes(study, top_n = 2.5),
    "positive integer"
  )
})


test_that("oh_plot_microbes rejects unavailable microbial data", {

  study <- create_visualization_study()

  study["microbial"] <- list(NULL)

  expect_error(
    oh_plot_microbes(study),
    "Microbial data are not available"
  )
})


test_that("oh_plot_microbes handles missing and blank identifiers", {

  study <- create_visualization_study()

  study$microbial$microorganism_id[1] <- NA
  study$microbial$microorganism_id[2] <- ""

  result <- oh_plot_microbes(study)

  expect_s3_class(result, "ggplot")
  expect_true(
    all(
      c("(Missing ID)", "(Blank ID)") %in%
        as.character(result$data$microorganism)
    )
  )
})


# ============================================================
# oh_plot_microbiological_status()
# ============================================================

test_that("oh_plot_microbiological_status returns a ggplot object", {

  study <- create_visualization_study()

  result <- oh_plot_microbiological_status(study)

  expect_s3_class(result, "ggplot")
})


test_that("oh_plot_microbiological_status preserves distinct statuses", {

  study <- create_visualization_study()

  result <- oh_plot_microbiological_status(study)

  expected_statuses <- c(
    "positive",
    "negative",
    "inconclusive",
    "not tested"
  )

  expect_setequal(
    as.character(result$data$status),
    expected_statuses
  )
})


test_that("oh_plot_microbiological_status rejects unavailable data", {

  study <- create_visualization_study()

  study["microbial"] <- list(NULL)

  expect_error(
    oh_plot_microbiological_status(study),
    "Microbial data are not available"
  )
})


# ============================================================
# oh_plot_environment()
# ============================================================

test_that("oh_plot_environment returns a ggplot object", {

  study <- create_visualization_study()

  result <- oh_plot_environment(
    study,
    parameter = "Temperature"
  )

  expect_s3_class(result, "ggplot")
})


test_that("oh_plot_environment rejects unknown parameters", {

  study <- create_visualization_study()

  expect_error(
    oh_plot_environment(study, "Unknown"),
    "No environmental measurements found"
  )
})


test_that("oh_plot_environment rejects invalid parameter arguments", {

  study <- create_visualization_study()

  expect_error(
    oh_plot_environment(study, ""),
    "non-empty character string"
  )

  expect_error(
    oh_plot_environment(study, NA_character_),
    "non-empty character string"
  )
})


test_that("oh_plot_environment rejects mixed units", {

  study <- create_visualization_study()

  study$environment$unit[2] <- "F"

  expect_error(
    oh_plot_environment(study, "Temperature"),
    "multiple or incompatible units"
  )
})


test_that("oh_plot_environment rejects non-numeric values", {

  study <- create_visualization_study()

  study$environment$value[1] <- "unknown"

  expect_error(
    oh_plot_environment(study, "Temperature"),
    "finite numeric values"
  )
})


test_that("oh_plot_environment rejects missing values", {

  study <- create_visualization_study()

  study$environment$value[1] <- NA_character_

  expect_error(
    oh_plot_environment(study, "Temperature"),
    "missing or blank values"
  )
})


test_that("oh_plot_environment rejects unavailable environmental data", {

  study <- create_visualization_study()

  study["environment"] <- list(NULL)

  expect_error(
    oh_plot_environment(study, "Temperature"),
    "Environmental data are not available"
  )
})
