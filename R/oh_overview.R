# ============================================================
# OneHealthVizR
# File: oh_overview.R
# Purpose: Generate a descriptive overview of OneHealthStudy
#          objects.
# ============================================================


#' Generate an overview of a OneHealthStudy object
#'
#' Summarizes the dimensions, missing values and distinct
#' identifiers of the available One Health data tables.
#'
#' @param x An object of class OneHealthStudy.
#'
#' @return An object of class OneHealthOverview containing:
#'   \item{study_id}{Study identifier.}
#'   \item{table_summary}{Number of records, variables and
#'   missing values in each table.}
#'   \item{identifier_summary}{Number of distinct identifiers
#'   in each available table.}
#'
#' @export

oh_overview <- function(x) {

  # 1. Validate input object

  oh_validate(x)


  # 2. Define available tables

  table_names <- c(
    "human",
    "animal",
    "environment",
    "microbial"
  )

  available_tables <- table_names[
    vapply(
      table_names,
      function(name) !is.null(x[[name]]),
      logical(1)
    )
  ]


  # 3. Initialize summary containers

  table_summary <- data.frame(
    table = character(),
    records = integer(),
    variables = integer(),
    missing_values = integer(),
    missing_percent = numeric(),
    stringsAsFactors = FALSE
  )

  identifier_summary <- data.frame(
    table = character(),
    identifier = character(),
    distinct_values = integer(),
    stringsAsFactors = FALSE
  )


  # 4. Define identifier columns

  identifier_columns <- c(
    "study_id",
    "host_id",
    "site_id",
    "event_id",
    "sample_id",
    "measurement_id",
    "observation_id",
    "microorganism_id"
  )


  # 5. Summarize each available table

  for (table_name in available_tables) {

    data <- x[[table_name]]

    n_records <- nrow(data)
    n_variables <- ncol(data)


    # Count missing values
    # NA and empty character values are considered missing.

    missing_matrix <- vapply(
      data,
      function(column) {

        missing <- is.na(column)

        if (is.character(column) || is.factor(column)) {
          missing <- missing |
            (!is.na(column) & !nzchar(trimws(as.character(column))))
        }

        missing

      },
      logical(n_records)
    )

    if (is.null(dim(missing_matrix))) {
      missing_matrix <- matrix(
        missing_matrix,
        ncol = 1L
      )
    }

    n_missing <- sum(missing_matrix)

    total_cells <- n_records * n_variables

    missing_percent <- if (total_cells > 0L) {
      round(100 * n_missing / total_cells, 2)
    } else {
      0
    }


    # Store table summary

    table_summary <- rbind(
      table_summary,
      data.frame(
        table = table_name,
        records = as.integer(n_records),
        variables = as.integer(n_variables),
        missing_values = as.integer(n_missing),
        missing_percent = missing_percent,
        stringsAsFactors = FALSE
      )
    )


    # Summarize distinct identifiers

    available_identifiers <- intersect(
      identifier_columns,
      names(data)
    )

    for (identifier in available_identifiers) {

      values <- as.character(data[[identifier]])

      values <- values[
        !is.na(values) &
          nzchar(trimws(values))
      ]

      n_distinct <- length(unique(values))

      identifier_summary <- rbind(
        identifier_summary,
        data.frame(
          table = table_name,
          identifier = identifier,
          distinct_values = as.integer(n_distinct),
          stringsAsFactors = FALSE
        )
      )

    }

  }


  # 6. Build overview object

  result <- list(
    study_id = x$study_id,
    table_summary = table_summary,
    identifier_summary = identifier_summary
  )

  class(result) <- "OneHealthOverview"

  result

}


#' Print a OneHealthOverview object
#'
#' @param x An object of class OneHealthOverview.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object.
#'
#' @export

print.OneHealthOverview <- function(x, ...) {

  cat("\n")
  cat("OneHealthVizR - Study overview\n")
  cat("----------------------------------------\n")

  cat("Study ID:", x$study_id, "\n\n")

  cat("Table summary:\n")
  print(
    x$table_summary,
    row.names = FALSE
  )

  cat("\nIdentifier summary:\n")
  print(
    x$identifier_summary,
    row.names = FALSE
  )

  cat("\n")

  invisible(x)

}
