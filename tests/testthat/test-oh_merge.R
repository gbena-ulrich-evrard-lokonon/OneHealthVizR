test_that("oh_merge preserves the original study tables", {

  study <- create_valid_study()

  merged <- oh_merge(study)

  expect_s3_class(merged, "OneHealthMerged")
  expect_identical(merged$study_id, study$study_id)
  expect_equal(merged$samples$sample_id, "SAMPLE001")
  expect_equal(merged$samples$compartment, "human")
  expect_null(merged$microbial)
  expect_null(merged$environment)

  expect_equal(study$human$sample_id, "SAMPLE001")
})


test_that("oh_merge rejects invalid study objects", {

  expect_error(
    oh_merge(list(study_id = "STUDY001")),
    "OneHealthStudy"
  )

})


test_that("oh_merge handles a study without human or animal data", {

  study <- create_valid_study()

  study["human"] <- list(NULL)
  study["animal"] <- list(NULL)

  merged <- oh_merge(study)

  expect_equal(nrow(merged$samples), 0L)
  expect_null(merged$microbial)
  expect_null(merged$environment)

})

test_that("oh_merge integrates a complete Streptococcus suis study", {

  study <- create_streptococcus_study()

  merged <- oh_merge(study)

  # Study identity
  expect_identical(merged$study_id, "STREP001")

  # Human and animal sample metadata
  expect_equal(nrow(merged$samples), 5L)
  expect_equal(
    sum(merged$samples$compartment == "human"),
    2L
  )
  expect_equal(
    sum(merged$samples$compartment == "animal"),
    3L
  )

  # Microbial observations must not be multiplied
  expect_equal(nrow(merged$microbial), 5L)
  expect_equal(
    length(unique(merged$microbial$observation_id)),
    5L
  )

  # Microbial observations must retain their compartment
  compartment_by_sample <- stats::setNames(
    merged$microbial$sample_compartment,
    merged$microbial$sample_id
  )

  expect_identical(
    unname(compartment_by_sample[c("H-S01", "H-S02")]),
    c("human", "human")
  )

  expect_identical(
    unname(compartment_by_sample[c("A-S01", "A-S02")]),
    c("animal", "animal")
  )

  expect_identical(
    unname(compartment_by_sample["E-S01"]),
    "environment"
  )

  # Repeated environmental measurements must be preserved
  expect_equal(nrow(merged$environment), 3L)
  expect_equal(
    length(unique(merged$environment$measurement_id)),
    3L
  )

  # Original study tables must remain unchanged
  expect_equal(
    merged$environment,
    study$environment
  )

  expect_equal(
    nrow(study$microbial),
    5L
  )
})

test_that("oh_merge handles a study without human data", {

  study <- create_streptococcus_study()
  study["human"] <- list(NULL)

  merged <- oh_merge(study)

  expect_equal(nrow(merged$samples), 3L)
  expect_true(all(merged$samples$compartment == "animal"))
  expect_equal(nrow(merged$microbial), 5L)

})


test_that("oh_merge handles a study without animal data", {

  study <- create_streptococcus_study()
  study["animal"] <- list(NULL)

  merged <- oh_merge(study)

  expect_equal(nrow(merged$samples), 2L)
  expect_true(all(merged$samples$compartment == "human"))
  expect_equal(nrow(merged$microbial), 5L)

})


test_that("oh_merge handles a study without environmental data", {

  study <- create_streptococcus_study()
  study["environment"] <- list(NULL)

  merged <- oh_merge(study)

  expect_null(merged$environment)
  expect_equal(nrow(merged$samples), 5L)
  expect_equal(nrow(merged$microbial), 5L)

})


test_that("oh_merge handles a study without microbial observations", {

  study <- create_streptococcus_study()
  study["microbial"] <- list(NULL)

  merged <- oh_merge(study)

  expect_null(merged$microbial)
  expect_equal(nrow(merged$samples), 5L)
  expect_equal(nrow(merged$environment), 3L)

})


test_that("oh_merge preserves multiple environmental measurements per sample", {

  study <- create_streptococcus_study()

  merged <- oh_merge(study)

  environmental_rows <- merged$environment[
    merged$environment$sample_id == "E-S01",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(environmental_rows), 3L)

  expect_setequal(
    environmental_rows$parameter,
    c("Temperature", "Humidity", "pH")
  )

  expect_equal(nrow(merged$microbial), 5L)

})


test_that("oh_merge preserves environmental measurements without sample IDs", {

  study <- create_streptococcus_study()

  # Remove the environmental sample reference
  study$environment$sample_id <- NA_character_

  # Remove the microbial observation linked to E-S01
  study$microbial <- study$microbial[
    study$microbial$sample_id != "E-S01",
    ,
    drop = FALSE
  ]

  merged <- oh_merge(study)

  expect_equal(nrow(merged$environment), 3L)
  expect_true(all(is.na(merged$environment$sample_id)))

  expect_equal(nrow(merged$microbial), 4L)

  expect_false(
    "environment" %in% merged$microbial$sample_compartment
  )

})

test_that("sample helper combines human and animal records", {

  study <- create_streptococcus_study()

  samples <- OneHealthVizR:::.oh_merge_samples(study)

  expect_equal(nrow(samples), 5)
  expect_equal(
    sum(samples$compartment == "human"),
    2
  )
  expect_equal(
    sum(samples$compartment == "animal"),
    3
  )
  expect_true("sample_id" %in% names(samples))
  expect_true("host_id" %in% names(samples))
})


test_that("sample helper handles a missing human table", {

  study <- create_streptococcus_study()
  study["human"] <- list(NULL)

  samples <- OneHealthVizR:::.oh_merge_samples(study)

  expect_equal(nrow(samples), 3)
  expect_true(all(samples$compartment == "animal"))
})


test_that("sample helper handles a missing animal table", {

  study <- create_streptococcus_study()
  study["animal"] <- list(NULL)

  samples <- OneHealthVizR:::.oh_merge_samples(study)

  expect_equal(nrow(samples), 2)
  expect_true(all(samples$compartment == "human"))
})


test_that("sample helper handles missing human and animal tables", {

  study <- create_streptococcus_study()
  study["human"] <- list(NULL)
  study["animal"] <- list(NULL)

  samples <- OneHealthVizR:::.oh_merge_samples(study)

  expect_equal(nrow(samples), 0)
  expect_equal(ncol(samples), 0)
})


test_that("sample helper rejects a reserved compartment column", {

  study <- create_streptococcus_study()
  study$human$compartment <- "custom"

  expect_error(
    OneHealthVizR:::.oh_merge_samples(study),
    "reserved column"
  )
})

test_that("environment helper preserves all measurements", {

  study <- create_streptococcus_study()

  environment <- OneHealthVizR:::.oh_merge_environment(study)

  expect_equal(nrow(environment), 3)

  expect_equal(
    environment$measurement_id,
    study$environment$measurement_id
  )

  expect_equal(
    environment$parameter,
    study$environment$parameter
  )
})


test_that("environment helper preserves relational identifiers", {

  study <- create_streptococcus_study()

  environment <- OneHealthVizR:::.oh_merge_environment(study)

  expected_columns <- c(
    "measurement_id",
    "study_id",
    "site_id",
    "event_id",
    "sample_id"
  )

  expect_true(
    all(expected_columns %in% names(environment))
  )

  expect_equal(
    environment$site_id,
    study$environment$site_id
  )

  expect_equal(
    environment$event_id,
    study$environment$event_id
  )
})


test_that("environment helper handles missing environmental data", {

  study <- create_streptococcus_study()
  study["environment"] <- list(NULL)

  environment <- OneHealthVizR:::.oh_merge_environment(study)

  expect_null(environment)
})


test_that("environment helper preserves repeated measurements", {

  study <- create_streptococcus_study()

  study$environment <- rbind(
    study$environment,
    study$environment[1, , drop = FALSE]
  )

  study$environment$measurement_id[4] <- "M004"
  study$environment$parameter[4] <- "Temperature"

  environment <- OneHealthVizR:::.oh_merge_environment(study)

  expect_equal(nrow(environment), 4)

  expect_equal(
    sum(environment$parameter == "Temperature"),
    2
  )
})


test_that("environment helper preserves missing sample identifiers", {

  study <- create_streptococcus_study()

  study$environment$sample_id <- NA_character_

  environment <- OneHealthVizR:::.oh_merge_environment(study)

  expect_true(
    all(is.na(environment$sample_id))
  )

  expect_equal(nrow(environment), 3)
})
