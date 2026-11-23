#' Import One Health study data
#'
#' Imports human, animal, environmental and microbiological observation
#' tables into a structured OneHealthStudy object.
#'
#' Input files must use the official OneHealthVizR column names.
#' CSV, XLS and XLSX files are supported.
#'
#' All imported columns are initially read as character data to avoid
#' unintended automatic conversion of identifiers, dates and measurements.
#' Original values are preserved.
#'
#' @param human Path to the human data file. Optional.
#' @param animal Path to the animal data file. Optional.
#' @param environment Path to the environmental measurements file. Optional.
#' @param microbial Path to the microbiological observations file. Optional.
#'
#' @return An object of class \code{OneHealthStudy}, containing the imported
#'   tables, study identifier and source file paths.
#'
#' @details
#' At least one input file must be provided. Every supplied table must
#' contain its required columns and exactly one non-missing study identifier.
#' Additional columns are preserved.
#'
#' When all three compartment tables are provided, sample identifiers
#' referenced by the microbial observations are checked against the
#' available compartment sample identifiers.
#'
#' @examples
#' \dontrun{
#' study <- oh_import(
#'   human = "human.csv",
#'   animal = "animal.csv",
#'   environment = "environment.csv",
#'   microbial = "microbial_observations.csv"
#' )
#' }
#'
#' @export
oh_import <- function(
    human = NULL,
    animal = NULL,
    environment = NULL,
    microbial = NULL
) {

  files <- list(
    human = human,
    animal = animal,
    environment = environment,
    microbial = microbial
  )

  supplied <- !vapply(files, is.null, logical(1))

  if (!any(supplied)) {
    stop(
      "At least one input file must be provided.",
      call. = FALSE
    )
  }

  schemas <- .oh_schema()
  imported <- list()

  for (table_name in names(files)[supplied]) {

    file_path <- files[[table_name]]

    if (!is.character(file_path) ||
        length(file_path) != 1L ||
        is.na(file_path) ||
        !nzchar(file_path)) {

      stop(
        sprintf("Invalid file path for '%s'.", table_name),
        call. = FALSE
      )
    }

    if (!file.exists(file_path) ||
        dir.exists(file_path)) {

      stop(
        sprintf(
          "File not found for '%s': %s",
          table_name,
          file_path
        ),
        call. = FALSE
      )
    }

    extension <- tolower(tools::file_ext(file_path))

    if (!extension %in% c("csv", "xls", "xlsx")) {
      stop(
        sprintf(
          "Unsupported file format for '%s'. Use CSV, XLS or XLSX.",
          table_name
        ),
        call. = FALSE
      )
    }

    data <- tryCatch({

      if (extension == "csv") {

        readr::read_csv(
          file_path,
          col_types = readr::cols(
            .default = readr::col_character()
          ),
          name_repair = "minimal",
          na = character(),
          trim_ws = FALSE,
          show_col_types = FALSE,
          progress = FALSE
        )

      } else {

        readxl::read_excel(
          file_path,
          col_types = "text",
          .name_repair = "minimal"
        )
      }

    }, error = function(e) {

      stop(
        sprintf(
          "Could not import '%s': %s",
          table_name,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    })

    if (nrow(data) == 0L) {
      stop(
        sprintf("The '%s' table contains no data rows.", table_name),
        call. = FALSE
      )
    }

    if (anyDuplicated(names(data))) {
      stop(
        sprintf(
          "The '%s' table contains duplicated column names.",
          table_name
        ),
        call. = FALSE
      )
    }

    missing_columns <- setdiff(
      schemas[[table_name]],
      names(data)
    )

    if (length(missing_columns) > 0L) {
      stop(
        sprintf(
          "The '%s' table is missing required columns: %s",
          table_name,
          paste(missing_columns, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    imported[[table_name]] <- as.data.frame(
      data,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  # Check study identifiers and primary keys.

  primary_keys <- c(
    human = "sample_id",
    animal = "sample_id",
    environment = "measurement_id",
    microbial = "observation_id"
  )

  study_ids <- character()

  for (table_name in names(imported)) {

    data <- imported[[table_name]]

    current_study_ids <- unique(data$study_id)

    if (anyNA(current_study_ids) ||
        any(!nzchar(trimws(current_study_ids)))) {

      stop(
        sprintf(
          "The '%s' table contains missing study_id values.",
          table_name
        ),
        call. = FALSE
      )
    }

    if (length(current_study_ids) != 1L) {
      stop(
        sprintf(
          "The '%s' table must contain exactly one study_id.",
          table_name
        ),
        call. = FALSE
      )
    }

    study_ids <- c(study_ids, current_study_ids)

    key_name <- primary_keys[[table_name]]
    key_values <- data[[key_name]]

    if (anyNA(key_values) ||
        any(!nzchar(trimws(key_values)))) {

      stop(
        sprintf(
          "The '%s' table contains missing values in '%s'.",
          table_name,
          key_name
        ),
        call. = FALSE
      )
    }

    if (anyDuplicated(key_values)) {
      stop(
        sprintf(
          "The '%s' table contains duplicated values in '%s'.",
          table_name,
          key_name
        ),
        call. = FALSE
      )
    }
  }

  if (length(unique(study_ids)) != 1L) {
    stop(
      "All supplied tables must belong to the same study_id.",
      call. = FALSE
    )
  }

  # Check sample identifiers across the three compartments.

  compartment_tables <- c(
    "human",
    "animal",
    "environment"
  )

  available_compartments <- intersect(
    compartment_tables,
    names(imported)
  )

  sample_ids_by_table <- lapply(
    available_compartments,
    function(table_name) {

      values <- imported[[table_name]]$sample_id

      values[
        !is.na(values) &
          nzchar(trimws(values))
      ] |> unique()
    }
  )

  if (length(sample_ids_by_table) > 1L) {

    for (i in seq_len(length(sample_ids_by_table) - 1L)) {

      for (j in (i + 1L):length(sample_ids_by_table)) {

        duplicated_ids <- intersect(
          sample_ids_by_table[[i]],
          sample_ids_by_table[[j]]
        )

        if (length(duplicated_ids) > 0L) {
          stop(
            paste0(
              "Sample identifiers must be unique across compartments. ",
              "Duplicated identifier(s): ",
              paste(duplicated_ids, collapse = ", ")
            ),
            call. = FALSE
          )
        }
      }
    }
  }

  # Check microbial sample references when all compartments are available.

  if (all(compartment_tables %in% names(imported)) &&
      "microbial" %in% names(imported)) {

    available_sample_ids <- unique(
      unlist(sample_ids_by_table, use.names = FALSE)
    )

    microbial_sample_ids <- imported$microbial$sample_id

    unknown_samples <- setdiff(
      microbial_sample_ids,
      available_sample_ids
    )

    if (length(unknown_samples) > 0L) {
      stop(
        paste0(
          "Some microbial observations reference unknown sample_id values: ",
          paste(unknown_samples, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  study_id <- unique(study_ids)

  result <- list(
    study_id = study_id,
    human = imported$human,
    animal = imported$animal,
    environment = imported$environment,
    microbial = imported$microbial,
    source_files = files[supplied]
  )

  class(result) <- c("OneHealthStudy", "list")

  result
}


#' Print a OneHealthStudy object
#'
#' Displays a concise summary of the imported One Health study.
#'
#' @param x An object of class \code{OneHealthStudy}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The supplied object, invisibly.
#'
#' @export
print.OneHealthStudy <- function(x, ...) {

  cat("OneHealthVizR - One Health Study\n")
  cat("--------------------------------\n")

  cat("Study ID:", x$study_id, "\n\n")

  table_names <- c(
    human = "Human",
    animal = "Animal",
    environment = "Environmental measurements",
    microbial = "Microbial observations"
  )

  for (table_name in names(table_names)) {

    data <- x[[table_name]]

    if (is.null(data)) {
      cat(table_names[[table_name]], ": Not provided\n")
    } else {
      cat(
        table_names[[table_name]],
        ": ",
        nrow(data),
        " rows and ",
        ncol(data),
        " columns\n",
        sep = ""
      )
    }
  }

  invisible(x)
}
