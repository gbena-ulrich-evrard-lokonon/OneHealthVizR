#' Select columns from a dataset
#'
#' @param data A data frame.
#' @param columns Optional character vector of column names.
#' @return A data frame containing the selected columns.
#' @keywords internal

.oh_select_columns <- function(data, columns = NULL) {

  if (is.null(data)) {
    return(NULL)
  }

  if (is.null(columns)) {
    return(data)
  }

  if (!is.character(columns) || length(columns) == 0L) {
    stop("`columns` must be a non-empty character vector.",
         call. = FALSE)
  }

  if (anyDuplicated(columns)) {
    stop("`columns` must not contain duplicated names.",
         call. = FALSE)
  }

  missing_columns <- setdiff(columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "Unknown columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  data[, columns, drop = FALSE]
}


#' Filter rows from a dataset
#'
#' @param data A data frame.
#' @param filters Optional named list of filtering values.
#' @return A filtered data frame.
#' @keywords internal

.oh_filter_rows <- function(data, filters = NULL) {

  if (is.null(data) || is.null(filters)) {
    return(data)
  }

  if (!is.list(filters) || length(filters) == 0L) {
    stop("`filters` must be a non-empty named list.",
         call. = FALSE)
  }

  if (
    is.null(names(filters)) ||
    any(names(filters) == "") ||
    anyDuplicated(names(filters))
  ) {
    stop("`filters` must have unique, non-empty column names.",
         call. = FALSE)
  }

  missing_columns <- setdiff(names(filters), names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "Unknown filter columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  keep <- rep(TRUE, nrow(data))

  for (column in names(filters)) {

    values <- filters[[column]]

    if (is.null(values) || length(values) == 0L) {
      stop(
        "Filter values for `", column, "` cannot be empty.",
        call. = FALSE
      )
    }

    if (anyNA(values)) {
      stop(
        "Filter values for `", column, "` cannot contain NA.",
        call. = FALSE
      )
    }

    keep <- keep & (
      !is.na(data[[column]]) &
        data[[column]] %in% values
    )
  }

  result <- data[keep, , drop = FALSE]
  rownames(result) <- NULL

  result
}


#' Transform numerical variables
#'
#' @param data A data frame.
#' @param transformations Named list of numerical transformations.
#' @return A transformed data frame.
#' @keywords internal

.oh_transform_numeric <- function(data, transformations = NULL) {

  if (is.null(data) || is.null(transformations)) {
    return(data)
  }

  if (!is.list(transformations) || length(transformations) == 0L) {
    stop(
      "`transformations` must be a non-empty named list.",
      call. = FALSE
    )
  }

  if (
    is.null(names(transformations)) ||
    any(names(transformations) == "") ||
    anyDuplicated(names(transformations))
  ) {
    stop(
      "`transformations` must have unique, non-empty column names.",
      call. = FALSE
    )
  }

  missing_columns <- setdiff(names(transformations), names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "Unknown transformation columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  for (column in names(transformations)) {

    method <- transformations[[column]]

    if (
      !is.character(method) ||
      length(method) != 1L ||
      is.na(method)
    ) {
      stop(
        "Transformation for `", column,
        "` must be a single character value.",
        call. = FALSE
      )
    }

    if (!is.numeric(data[[column]])) {
      stop(
        "Column `", column, "` must be numeric.",
        call. = FALSE
      )
    }

    if (any(!is.finite(data[[column]]) & !is.na(data[[column]]))) {
      stop(
        "Column `", column, "` contains non-finite values.",
        call. = FALSE
      )
    }

    transformed <- switch(
      method,

      log = {
        if (any(data[[column]] <= 0, na.rm = TRUE)) {
          stop(
            "Log transformation requires strictly positive values in `",
            column, "`.",
            call. = FALSE
          )
        }
        log(data[[column]])
      },

      log1p = {
        if (any(data[[column]] < -1, na.rm = TRUE)) {
          stop(
            "Log1p transformation requires values greater than or equal to -1 in `",
            column, "`.",
            call. = FALSE
          )
        }
        log1p(data[[column]])
      },

      sqrt = {
        if (any(data[[column]] < 0, na.rm = TRUE)) {
          stop(
            "Square root transformation requires non-negative values in `",
            column, "`.",
            call. = FALSE
          )
        }
        sqrt(data[[column]])
      },

      scale = as.numeric(scale(data[[column]])),

      stop(
        "Unsupported transformation `", method,
        "`. Choose: log, log1p, sqrt or scale.",
        call. = FALSE
      )
    )

    data[[column]] <- transformed
  }

  data
}


#' Transform an integrated One Health dataset
#'
#' Selects columns, filters observations and applies numerical
#' transformations to a selected dataset.
#'
#' @param x A OneHealthMerged object.
#' @param dataset Dataset to transform: samples, microbial or environment.
#' @param select Optional character vector of columns to retain.
#' @param filters Optional named list of filtering values.
#' @param numeric_transform Optional named list of numerical transformations.
#'
#' @return A OneHealthTransformed object containing the transformed dataset
#' and transformation metadata.
#' @export

oh_transform <- function(
    x,
    dataset = c("samples", "microbial", "environment"),
    select = NULL,
    filters = NULL,
    numeric_transform = NULL
) {

  if (!inherits(x, "OneHealthMerged")) {
    stop("`x` must be a OneHealthMerged object.",
         call. = FALSE)
  }

  dataset <- match.arg(dataset)
  data <- x[[dataset]]

  if (is.null(data)) {
    stop(
      "Dataset `", dataset, "` is not available in this object.",
      call. = FALSE
    )
  }

  if (!is.data.frame(data)) {
    stop(
      "Dataset `", dataset, "` must be a data frame.",
      call. = FALSE
    )
  }

  original_nrow <- nrow(data)

  data <- .oh_filter_rows(data, filters)
  data <- .oh_transform_numeric(data, numeric_transform)
  data <- .oh_select_columns(data, select)

  transformed_data <- data

  result <- list(
    study_id = x$study_id,
    dataset = dataset,
    data = transformed_data,
    metadata = list(
      original_rows = original_nrow,
      transformed_rows = nrow(transformed_data),
      selected_columns = names(transformed_data),
      filters = filters,
      numeric_transformations = numeric_transform
    )
  )

  class(result) <- c("OneHealthTransformed", "list")

  result
}
