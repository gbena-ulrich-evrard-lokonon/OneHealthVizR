#' Define the official input data schemas
#'
#' Returns the required column names for each OneHealthVizR input table.
#' Additional columns are permitted and preserved during import.
#'
#' @return A named list containing the required columns for each table.
#' @keywords internal
.oh_schema <- function() {

  list(

    human = c(
      "study_id",
      "host_id",
      "site_id",
      "event_id",
      "sample_id",
      "age",
      "sex",
      "occupation",
      "sample_type",
      "collection_date",
      "infection_status"
    ),

    animal = c(
      "study_id",
      "host_id",
      "site_id",
      "event_id",
      "sample_id",
      "species",
      "age",
      "sex",
      "production_type",
      "sample_type",
      "collection_date",
      "infection_status"
    ),

    environment = c(
      "measurement_id",
      "study_id",
      "site_id",
      "event_id",
      "sample_id",
      "parameter",
      "value",
      "unit",
      "measurement_date",
      "method"
    ),

    microbial = c(
      "observation_id",
      "study_id",
      "sample_id",
      "microorganism_id",
      "method",
      "result_type",
      "result_status",
      "result_value",
      "result_unit"
    )

  )
}
