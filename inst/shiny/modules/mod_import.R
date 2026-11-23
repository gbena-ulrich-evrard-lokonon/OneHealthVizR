# ============================================================
# OneHealthVizR - Import module
# Purpose:
# Provide the Shiny interface for importing One Health tables.
# Scientific import and validation are delegated to oh_import().
# ============================================================


#' User interface for the data import module
#'
#' @param id Shiny module identifier.
#'
#' @return A Shiny UI component.
mod_import_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::h3("Import One Health data"),

    shiny::p(
      "Select the files corresponding to your study. ",
      "At least one file must be provided."
    ),

    shiny::fileInput(
      ns("human"),
      "Human data (CSV, XLS, XLSX)",
      accept = c(".csv", ".xls", ".xlsx")
    ),

    shiny::fileInput(
      ns("animal"),
      "Animal data (CSV, XLS, XLSX)",
      accept = c(".csv", ".xls", ".xlsx")
    ),

    shiny::fileInput(
      ns("environment"),
      "Environmental measurements (CSV, XLS, XLSX)",
      accept = c(".csv", ".xls", ".xlsx")
    ),

    shiny::fileInput(
      ns("microbial"),
      "Microbiological observations (CSV, XLS, XLSX)",
      accept = c(".csv", ".xls", ".xlsx")
    ),

    shiny::actionButton(
      ns("import"),
      "Import study",
      class = "btn-primary"
    ),

    shiny::hr(),

    shiny::h4("Import summary"),

    shiny::verbatimTextOutput(
      ns("summary")
    )

  )
}


#' Server logic for the data import module
#'
#' Calls the OneHealthVizR import function and stores the resulting
#' OneHealthStudy object in a reactive value.
#'
#' @param id Shiny module identifier.
#'
#' @return A reactive expression containing the imported study.
mod_import_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    imported_study <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$import, {

      get_file_path <- function(file_input) {

        if (is.null(file_input)) {
          return(NULL)
        }

        file_input$datapath
      }

      tryCatch({

        study <- OneHealthVizR::oh_import(

          human = get_file_path(input$human),

          animal = get_file_path(input$animal),

          environment = get_file_path(input$environment),

          microbial = get_file_path(input$microbial)

        )

        imported_study(study)

        shiny::showNotification(
          "Study imported successfully.",
          type = "message"
        )

      }, error = function(e) {

        imported_study(NULL)

        shiny::showNotification(
          conditionMessage(e),
          type = "error",
          duration = NULL
        )

      })

    })

    output$summary <- shiny::renderPrint({

      study <- imported_study()

      if (is.null(study)) {
        cat("No study has been imported yet.")
      } else {
        print(study)
      }

    })

    return(shiny::reactive(imported_study()))

  })
}
