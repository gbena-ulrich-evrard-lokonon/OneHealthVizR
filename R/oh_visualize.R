# ============================================================
# OneHealthVizR
# File: oh_visualize.R
# Purpose: Visualize One Health data
# ============================================================


# Declare ggplot2 aesthetic variables
utils::globalVariables(
  c(
    "compartment",
    "records",
    "value",
    "microorganism",
    "observations",
    "status"
  )
)


# ============================================================
# oh_plot_compartments()
# ============================================================

#' Plot the distribution of records across One Health compartments
#'
#' @param x A valid OneHealthStudy object.
#'
#' @return A ggplot object showing the number of records in each
#' available One Health compartment.
#'
#' @details
#' The function displays record counts for human, animal,
#' environmental and microbial data.
#'
#' These counts represent records in each table and should not
#' be interpreted as equivalent numbers of individuals or samples.
#'
#' @export
oh_plot_compartments <- function(x) {

  # Validate input object
  oh_validate(x)

  # Define available compartments
  compartments <- c(
    human = "Human records",
    animal = "Animal records",
    environment = "Environmental measurements",
    microbial = "Microbial observations"
  )

  # Identify available tables
  available_tables <- names(compartments)[
    vapply(
      names(compartments),
      function(table_name) {
        is.data.frame(x[[table_name]])
      },
      logical(1)
    )
  ]

  # Check whether at least one table is available
  if (length(available_tables) == 0) {
    stop(
      "No data are available for visualization.",
      call. = FALSE
    )
  }

  # Count records in each available table
  record_counts <- vapply(
    available_tables,
    function(table_name) {
      nrow(x[[table_name]])
    },
    integer(1)
  )

  # Prepare visualization data
  plot_data <- data.frame(
    compartment = unname(compartments[available_tables]),
    records = unname(record_counts)
  )

  # Preserve the order of compartments
  plot_data$compartment <- factor(
    plot_data$compartment,
    levels = plot_data$compartment
  )

  # Generate plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = compartment,
      y = records
    )
  ) +
    ggplot2::geom_col(
      width = 0.7
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "One Health data distribution",
      subtitle = paste(
        "Study ID:",
        x$study_id
      ),
      x = NULL,
      y = "Number of records"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 14
      ),
      axis.text = ggplot2::element_text(
        size = 10
      )
    )

  return(p)
}


# ============================================================
# oh_plot_microbes()
# ============================================================

#' Plot the most frequently observed microorganisms
#'
#' @param x A valid OneHealthStudy object.
#' @param top_n Number of microorganisms to display. Defaults to 10.
#'
#' @return A ggplot object showing the number of observations
#' for the most frequently recorded microorganisms.
#'
#' @details
#' Counts are based on microorganism_id in the microbial table.
#' Missing and blank identifiers are displayed separately.
#' The function does not infer taxonomic identity or abundance.
#'
#' @export
oh_plot_microbes <- function(x, top_n = 10) {

  # Validate input object
  oh_validate(x)

  # Validate top_n
  if (
    !is.numeric(top_n) ||
    length(top_n) != 1 ||
    is.na(top_n) ||
    !is.finite(top_n) ||
    top_n < 1 ||
    top_n != floor(top_n)
  ) {
    stop(
      "'top_n' must be a single positive integer.",
      call. = FALSE
    )
  }

  # Check microbial data availability
  if (!is.data.frame(x$microbial) || nrow(x$microbial) == 0) {
    stop(
      "Microbial data are not available for visualization.",
      call. = FALSE
    )
  }

  # Extract microorganism identifiers
  microorganism_ids <- as.character(
    x$microbial$microorganism_id
  )

  # Preserve missing and blank identifiers as separate categories
  microorganism_labels <- ifelse(
    is.na(microorganism_ids),
    "(Missing ID)",
    ifelse(
      microorganism_ids == "",
      "(Blank ID)",
      microorganism_ids
    )
  )

  # Count observations per microorganism
  observation_counts <- as.data.frame(
    table(microorganism_labels),
    stringsAsFactors = FALSE
  )

  names(observation_counts) <- c(
    "microorganism",
    "observations"
  )

  # Order by descending observation count
  observation_counts <- observation_counts[
    order(
      -observation_counts$observations,
      observation_counts$microorganism
    ),
    ,
    drop = FALSE
  ]

  # Keep the top N microorganisms
  n_to_keep <- min(
    as.integer(top_n),
    nrow(observation_counts)
  )

  observation_counts <- observation_counts[
    seq_len(n_to_keep),
    ,
    drop = FALSE
  ]

  # Remove unused factor levels
  observation_counts$microorganism <- factor(
    observation_counts$microorganism,
    levels = rev(observation_counts$microorganism)
  )

  # Generate plot
  p <- ggplot2::ggplot(
    observation_counts,
    ggplot2::aes(
      x = microorganism,
      y = observations
    )
  ) +
    ggplot2::geom_col(
      width = 0.7
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Most frequently observed microorganisms",
      subtitle = paste(
        "Study ID:",
        x$study_id
      ),
      x = NULL,
      y = "Number of observations"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 14
      ),
      axis.text = ggplot2::element_text(
        size = 10
      )
    )

  return(p)
}


# ============================================================
# oh_plot_microbiological_status()
# ============================================================

#' Plot the distribution of microbiological result statuses
#'
#' @param x A valid OneHealthStudy object.
#'
#' @return A ggplot object showing the distribution of
#' microbiological result statuses.
#'
#' @details
#' Counts are based on result_status in the microbial table.
#' Missing and blank statuses are displayed separately.
#' No inference is made from missing observations.
#'
#' @export
oh_plot_microbiological_status <- function(x) {

  # Validate input object
  oh_validate(x)

  # Check microbial data availability
  if (!is.data.frame(x$microbial) || nrow(x$microbial) == 0) {
    stop(
      "Microbial data are not available for visualization.",
      call. = FALSE
    )
  }

  # Extract result statuses
  statuses <- as.character(
    x$microbial$result_status
  )

  # Preserve missing and blank statuses separately
  status_labels <- ifelse(
    is.na(statuses),
    "(Missing status)",
    ifelse(
      trimws(statuses) == "",
      "(Blank status)",
      statuses
    )
  )

  # Count observations per status
  status_counts <- as.data.frame(
    table(status_labels),
    stringsAsFactors = FALSE
  )

  names(status_counts) <- c(
    "status",
    "observations"
  )

  # Order statuses by descending frequency
  status_counts <- status_counts[
    order(
      -status_counts$observations,
      status_counts$status
    ),
    ,
    drop = FALSE
  ]

  # Preserve display order
  status_counts$status <- factor(
    status_counts$status,
    levels = rev(status_counts$status)
  )

  # Generate plot
  p <- ggplot2::ggplot(
    status_counts,
    ggplot2::aes(
      x = status,
      y = observations
    )
  ) +
    ggplot2::geom_col(
      width = 0.7
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Microbiological result status distribution",
      subtitle = paste(
        "Study ID:",
        x$study_id
      ),
      x = NULL,
      y = "Number of observations"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 14
      ),
      axis.text = ggplot2::element_text(
        size = 10
      )
    )

  return(p)
}


# ============================================================
# oh_plot_environment()
# ============================================================

#' Plot the distribution of an environmental parameter
#'
#' @param x A valid OneHealthStudy object.
#' @param parameter Character string specifying the environmental
#' parameter to visualize.
#'
#' @return A ggplot object showing the distribution of the selected
#' environmental parameter.
#'
#' @details
#' Values must be numeric and finite. All selected measurements
#' must use the same unit. Missing, blank and non-numeric values
#' are not silently removed or converted.
#'
#' @export
oh_plot_environment <- function(x, parameter) {

  # Validate input object
  oh_validate(x)

  # Validate parameter argument
  if (
    !is.character(parameter) ||
    length(parameter) != 1 ||
    is.na(parameter) ||
    trimws(parameter) == ""
  ) {
    stop(
      "'parameter' must be a single non-empty character string.",
      call. = FALSE
    )
  }

  # Check environmental data availability
  if (
    !is.data.frame(x$environment) ||
    nrow(x$environment) == 0
  ) {
    stop(
      "Environmental data are not available for visualization.",
      call. = FALSE
    )
  }

  # Select the requested parameter
  selected_data <- x$environment[
    !is.na(x$environment$parameter) &
      x$environment$parameter == parameter,
    ,
    drop = FALSE
  ]

  # Check whether the parameter exists
  if (nrow(selected_data) == 0) {
    stop(
      paste0(
        "No environmental measurements found for parameter '",
        parameter,
        "'."
      ),
      call. = FALSE
    )
  }

  # Extract and validate units
  units <- as.character(selected_data$unit)

  units[
    is.na(units) | trimws(units) == ""
  ] <- "(Unspecified unit)"

  unique_units <- unique(units)

  if (length(unique_units) != 1) {
    stop(
      paste0(
        "Selected measurements contain multiple or incompatible ",
        "units: ",
        paste(unique_units, collapse = ", "),
        ". Select data with a consistent unit."
      ),
      call. = FALSE
    )
  }

  # Extract environmental values
  raw_values <- as.character(selected_data$value)

  # Reject missing or blank values
  if (
    any(
      is.na(raw_values) |
      trimws(raw_values) == ""
    )
  ) {
    stop(
      "Selected environmental measurements contain missing or blank values.",
      call. = FALSE
    )
  }

  # Convert values to numeric
  numeric_values <- suppressWarnings(
    as.numeric(trimws(raw_values))
  )

  # Reject non-numeric or non-finite values
  if (
    any(
      is.na(numeric_values) |
      !is.finite(numeric_values)
    )
  ) {
    stop(
      "Selected environmental values must all be finite numeric values.",
      call. = FALSE
    )
  }

  # Prepare plotting data
  plot_data <- data.frame(
    value = numeric_values
  )

  # Generate unit label
  unit_label <- unique_units[[1]]

  if (unit_label == "(Unspecified unit)") {
    x_label <- paste(parameter, "(unit unspecified)")
  } else {
    x_label <- paste0(
      parameter,
      " (",
      unit_label,
      ")"
    )
  }

  # Generate plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = value)
  ) +
    ggplot2::geom_histogram(
      bins = 30,
      fill = "steelblue",
      color = "white"
    ) +
    ggplot2::labs(
      title = paste(
        "Environmental distribution:",
        parameter
      ),
      subtitle = paste(
        "Study ID:",
        x$study_id
      ),
      x = x_label,
      y = "Number of measurements"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 14
      ),
      axis.text = ggplot2::element_text(
        size = 10
      )
    )

  return(p)
}
