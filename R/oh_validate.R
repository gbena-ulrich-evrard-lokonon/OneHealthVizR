# ============================================================
# OneHealthVizR
# File: oh_validate.R
# Purpose: Validate OneHealthStudy objects, data structures
#          and relational integrity.
# ============================================================


#' Validate a OneHealthStudy object
#'
#' Validates the structure, required columns, identifiers
#' and relational consistency of a OneHealthStudy object.
#'
#' @param x An object of class OneHealthStudy.
#'
#' @return Invisibly returns TRUE if validation succeeds.
#'
#' @export

oh_validate <- function(x) {

  # 1. Validate object class

  if (!inherits(x, "OneHealthStudy")) {
    stop(
      "Object must inherit from 'OneHealthStudy'.",
      call. = FALSE
    )
  }


  # 2. Validate required object components

  required_components <- c(
    "study_id",
    "human",
    "animal",
    "environment",
    "microbial"
  )

  missing_components <- setdiff(
    required_components,
    names(x)
  )

  if (length(missing_components) > 0L) {
    stop(
      "Object has missing components: ",
      paste(missing_components, collapse = ", "),
      call. = FALSE
    )
  }


  # 3. Validate study identifier

  if (
    length(x$study_id) != 1L ||
    is.na(x$study_id) ||
    !nzchar(trimws(as.character(x$study_id)))
  ) {
    stop(
      "'study_id' must be a single non-empty value.",
      call. = FALSE
    )
  }

  study_id <- as.character(x$study_id)


  # 4. Retrieve official schemas

  schemas <- .oh_schema()

  tables <- c(
    "human",
    "animal",
    "environment",
    "microbial"
  )

  primary_keys <- c(
    human = "sample_id",
    animal = "sample_id",
    environment = "measurement_id",
    microbial = "observation_id"
  )


  # 5. Validate available tables

  for (table_name in tables) {

    data <- x[[table_name]]

    if (is.null(data)) {
      next
    }

    if (!is.data.frame(data)) {
      stop(
        "Component '",
        table_name,
        "' must be a data frame or NULL.",
        call. = FALSE
      )
    }

    if (nrow(data) == 0L) {
      stop(
        "Component '",
        table_name,
        "' contains no observations.",
        call. = FALSE
      )
    }


    # Validate column names

    if (
      is.null(names(data)) ||
      anyNA(names(data)) ||
      any(!nzchar(trimws(names(data))))
    ) {
      stop(
        "Invalid column names in '",
        table_name,
        "'.",
        call. = FALSE
      )
    }

    if (anyDuplicated(names(data))) {
      stop(
        "Duplicated column names in '",
        table_name,
        "'.",
        call. = FALSE
      )
    }


    # Validate required columns

    missing_columns <- setdiff(
      schemas[[table_name]],
      names(data)
    )

    if (length(missing_columns) > 0L) {
      stop(
        "Table has missing required columns in '",
        table_name,
        "': ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }


    # Validate study identifier consistency

    table_study_ids <- unique(
      as.character(data$study_id)
    )

    if (
      length(table_study_ids) != 1L ||
      is.na(table_study_ids) ||
      !identical(table_study_ids, study_id)
    ) {
      stop(
        "Study ID is inconsistent with the study object in '",
        table_name,
        "'.",
        call. = FALSE
      )
    }


    # Validate primary keys

    key <- primary_keys[[table_name]]

    key_values <- as.character(data[[key]])

    if (
      anyNA(key_values) ||
      any(!nzchar(trimws(key_values)))
    ) {
      stop(
        "Primary key contains missing values in '",
        key,
        "' in '",
        table_name,
        "'.",
        call. = FALSE
      )
    }

    if (anyDuplicated(key_values)) {
      stop(
        "Primary key contains duplicated values in '",
        key,
        "' in '",
        table_name,
        "'.",
        call. = FALSE
      )
    }

  }


  # 6. Validate sample identifiers

  human_ids <- if (!is.null(x$human)) {
    as.character(x$human$sample_id)
  } else {
    character()
  }

  animal_ids <- if (!is.null(x$animal)) {
    as.character(x$animal$sample_id)
  } else {
    character()
  }

  environment_ids <- if (!is.null(x$environment)) {
    as.character(x$environment$sample_id)
  } else {
    character()
  }

  biological_ids <- c(
    human_ids,
    animal_ids
  )

  biological_ids <- biological_ids[
    !is.na(biological_ids) &
      nzchar(trimws(biological_ids))
  ]

  environment_ids <- environment_ids[
    !is.na(environment_ids) &
      nzchar(trimws(environment_ids))
  ]


  # Human and animal sample IDs must be unique across both tables

  if (anyDuplicated(biological_ids)) {
    stop(
      "'sample_id' values must be unique across human and animal tables.",
      call. = FALSE
    )
  }


  # Environmental sample references must not duplicate
  # biological sample identifiers

  if (
    length(intersect(
      biological_ids,
      environment_ids
    )) > 0L
  ) {
    stop(
      "Environmental sample identifiers must not duplicate human or animal sample identifiers.",
      call. = FALSE
    )
  }


  # 7. Validate microbiological sample references

  if (!is.null(x$microbial)) {

    microbial_sample_ids <- unique(
      as.character(x$microbial$sample_id)
    )

    microbial_sample_ids <- microbial_sample_ids[
      !is.na(microbial_sample_ids) &
        nzchar(trimws(microbial_sample_ids))
    ]

    known_sample_ids <- c(
      biological_ids,
      environment_ids
    )


    # Check references only when all four tables
    # are available as data frames.

    all_components_available <- all(
      vapply(
        tables,
        function(name) is.data.frame(x[[name]]),
        logical(1)
      )
    )

    if (all_components_available) {

      unknown_ids <- setdiff(
        microbial_sample_ids,
        known_sample_ids
      )

      if (length(unknown_ids) > 0L) {
        stop(
          "Unknown sample_id references in microbial data: ",
          paste(unknown_ids, collapse = ", "),
          call. = FALSE
        )
      }

    }

  }


  # 8. Validation successful

  invisible(TRUE)

}
