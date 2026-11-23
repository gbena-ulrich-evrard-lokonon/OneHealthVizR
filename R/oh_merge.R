# ============================================================
# OneHealthVizR
# File: oh_merge.R
# Purpose: Integrate One Health data tables while preserving
#          their original relational structure.
# ============================================================


#' Integrate One Health study tables
#'
#' Integrates available human, animal, environmental and
#' microbiological data without modifying the original tables.
#'
#' @param x An object of class OneHealthStudy.
#'
#' @return An object of class OneHealthMerged.
#'
#' @export

oh_merge <- function(x) {

  # 1. Validate input

  oh_validate(x)


  # 2. Merge human and animal samples

  samples <- .oh_merge_samples(x)


  # 3. Merge microbiological observations

  microbial <- .oh_merge_microbes(
    x = x,
    samples = samples
  )


  # 4. Preserve environmental measurements

  environment <- .oh_merge_environment(x)


  # 5. Build integrated object

  result <- list(
    study_id = x$study_id,
    samples = samples,
    microbial = microbial,
    environment = environment,
    source = x
  )

  class(result) <- c(
    "OneHealthMerged",
    "list"
  )

  result
}


# ============================================================
# Internal helper: Merge human and animal sample metadata
# ============================================================

#' Merge human and animal sample metadata
#'
#' @param x A validated OneHealthStudy object.
#' @return A combined sample data frame.
#' @keywords internal

.oh_merge_samples <- function(x) {

  human <- x$human
  animal <- x$animal

  if (!is.null(human) && "compartment" %in% names(human)) {
    stop(
      "'compartment' is a reserved column name.",
      call. = FALSE
    )
  }

  if (!is.null(animal) && "compartment" %in% names(animal)) {
    stop(
      "'compartment' is a reserved column name.",
      call. = FALSE
    )
  }

  if (is.null(human) && is.null(animal)) {
    return(data.frame())
  }

  if (is.null(human)) {
    animal$compartment <- "animal"
    return(animal)
  }

  if (is.null(animal)) {
    human$compartment <- "human"
    return(human)
  }

  all_columns <- union(
    names(human),
    names(animal)
  )

  human$compartment <- "human"
  animal$compartment <- "animal"

  all_columns <- union(
    all_columns,
    "compartment"
  )

  for (column in all_columns) {

    if (!column %in% names(human)) {
      human[[column]] <- NA_character_
    }

    if (!column %in% names(animal)) {
      animal[[column]] <- NA_character_
    }
  }

  human <- human[, all_columns, drop = FALSE]
  animal <- animal[, all_columns, drop = FALSE]

  samples <- rbind(human, animal)

  rownames(samples) <- NULL

  samples
}


# ============================================================
# Internal helper: Merge microbiological observations
# ============================================================

#' Link microbiological observations to sample metadata
#'
#' @param x A validated OneHealthStudy object.
#' @param samples A data frame containing integrated sample metadata.
#' @return A data frame of microbiological observations, or NULL.
#' @keywords internal

.oh_merge_microbes <- function(x, samples) {

  if (is.null(x$microbial)) {
    return(NULL)
  }

  microbial <- x$microbial


  # Link observations to human and animal sample metadata

  if (nrow(samples) > 0L) {

    sample_index <- match(
      microbial$sample_id,
      samples$sample_id
    )

    sample_metadata <- samples[
      ,
      setdiff(
        names(samples),
        c("study_id", "sample_id")
      ),
      drop = FALSE
    ]

    names(sample_metadata) <- paste0(
      "sample_",
      names(sample_metadata)
    )

    sample_metadata <- sample_metadata[
      sample_index,
      ,
      drop = FALSE
    ]

    rownames(sample_metadata) <- NULL

    microbial <- cbind(
      microbial,
      sample_metadata
    )

  } else {

    microbial$sample_compartment <- NA_character_
  }


  # Identify environmental sample references

  if (!is.null(x$environment)) {

    environmental_sample_ids <- unique(
      as.character(x$environment$sample_id)
    )

    environmental_sample_ids <- environmental_sample_ids[
      !is.na(environmental_sample_ids) &
        nzchar(trimws(environmental_sample_ids))
    ]

    if (!"sample_compartment" %in% names(microbial)) {
      microbial$sample_compartment <- NA_character_
    }

    microbial$sample_compartment[
      microbial$sample_id %in% environmental_sample_ids
    ] <- "environment"
  }

  microbial
}


# ============================================================
# Internal helper: Preserve environmental measurements
# ============================================================

#' Prepare environmental measurements for integration
#'
#' Preserves environmental measurements in their original
#' long format while retaining their relational identifiers.
#'
#' @param x A validated OneHealthStudy object.
#' @return A data frame of environmental measurements, or NULL.
#' @keywords internal

.oh_merge_environment <- function(x) {

  if (is.null(x$environment)) {
    return(NULL)
  }

  # Retrieve the original environmental table
  environment <- x$environment

  # Check required relational columns
  required_keys <- c(
    "measurement_id",
    "study_id",
    "site_id",
    "event_id",
    "sample_id"
  )

  missing_keys <- setdiff(
    required_keys,
    names(environment)
  )

  if (length(missing_keys) > 0L) {
    stop(
      "Missing environmental relational columns: ",
      paste(missing_keys, collapse = ", "),
      call. = FALSE
    )
  }

  # Preserve original row order
  rownames(environment) <- NULL

  environment
}

