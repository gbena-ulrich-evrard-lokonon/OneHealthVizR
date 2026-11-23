# ============================================================
# OneHealthVizR
# File: inst/shiny/app.R
# Purpose: Scientific dashboard interface
# ============================================================

options(shiny.maxRequestSize = 100 * 1024^2)

library(shiny)

# ------------------------------------------------------------
# Theme
# ------------------------------------------------------------

onehealth_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');

body {
  font-family: 'Inter', sans-serif;
  background-color: #F4F7FA;
  color: #243447;
}

.navbar {
  background-color: #102A43 !important;
  border: none;
  border-radius: 0;
  margin-bottom: 0;
}

.navbar-brand {
  color: white !important;
  font-weight: 800;
  font-size: 21px;
}

.navbar-nav > li > a {
  color: #D9EAF2 !important;
}

.navbar-nav > .active > a,
.navbar-nav > .active > a:hover {
  background-color: #167D8D !important;
}

.dashboard-header {
  background: linear-gradient(120deg, #102A43, #167D8D);
  color: white;
  padding: 30px;
  border-radius: 14px;
  margin: 22px 0;
}

.dashboard-header h2 {
  font-weight: 800;
  margin-top: 0;
}

.dashboard-header p {
  color: #D9F1F2;
  margin-bottom: 0;
}

.brand-logo {
  max-height: 75px;
  max-width: 110px;
  object-fit: contain;
  background: white;
  padding: 6px;
  border-radius: 10px;
}

.sidebar-panel {
  background: white;
  padding: 20px;
  border-radius: 12px;
  box-shadow: 0 2px 10px rgba(16,42,67,0.06);
}

.content-panel {
  background: white;
  padding: 22px;
  border-radius: 12px;
  margin-bottom: 20px;
  box-shadow: 0 2px 10px rgba(16,42,67,0.06);
}

.section-title {
  color: #102A43;
  font-weight: 700;
  margin-top: 5px;
  margin-bottom: 18px;
}

.stat-card {
  background: white;
  border-radius: 12px;
  padding: 20px;
  margin-bottom: 15px;
  border-left: 5px solid #167D8D;
  box-shadow: 0 2px 10px rgba(16,42,67,0.06);
}

.stat-label {
  color: #627D98;
  font-size: 13px;
  font-weight: 600;
  text-transform: uppercase;
}

.stat-value {
  color: #102A43;
  font-size: 27px;
  font-weight: 800;
  margin-top: 8px;
}

.btn-primary {
  background-color: #167D8D;
  border-color: #167D8D;
  font-weight: 600;
}

.btn-primary:hover {
  background-color: #116675;
  border-color: #116675;
}

.footer {
  margin-top: 35px;
  padding: 24px;
  background: #102A43;
  color: #D9EAF2;
  text-align: center;
  border-radius: 12px 12px 0 0;
}

.footer strong {
  color: white;
}

.footer small {
  color: #A9C4D2;
}

.dataTables_wrapper {
  overflow-x: auto;
}
"

# ------------------------------------------------------------
# User interface
# ------------------------------------------------------------

ui <- fluidPage(

  tags$head(
    tags$style(HTML(onehealth_css))
  ),

  navbarPage(

    title = "OneHealthVizR",

    id = "main_navigation",

    header = NULL,

    tabPanel(
      "Dashboard",
      icon = icon("chart-pie")
    ),

    tabPanel(
      "Data import",
      icon = icon("file-import")
    ),

    tabPanel(
      "Visualizations",
      icon = icon("chart-column")
    ),

    tabPanel(
      "About",
      icon = icon("circle-info")
    ),

    footer = NULL
  ),

  # ----------------------------------------------------------
  # Dashboard header
  # ----------------------------------------------------------

  div(
    class = "container-fluid",

    div(
      class = "dashboard-header",

      fluidRow(

        column(
          width = 9,

          h2("OneHealthVizR"),

          p(
            "An integrated framework for One Health data ",
            "management, validation and visualization."
          )
        ),

        column(
          width = 3,

          uiOutput("brand_logo")
        )
      )
    ),

    # --------------------------------------------------------
    # Dashboard content
    # --------------------------------------------------------

    conditionalPanel(
      condition = "input.main_navigation == 'Dashboard'",

      fluidRow(

        column(
          width = 12,

          h3(
            "Study dashboard",
            class = "section-title"
          )
        )
      ),

      fluidRow(

        column(
          width = 3,

          div(
            class = "stat-card",

            div(
              class = "stat-label",
              "Human records"
            ),

            textOutput("human_count", inline = TRUE)
          )
        ),

        column(
          width = 3,

          div(
            class = "stat-card",

            div(
              class = "stat-label",
              "Animal records"
            ),

            textOutput("animal_count", inline = TRUE)
          )
        ),

        column(
          width = 3,

          div(
            class = "stat-card",

            div(
              class = "stat-label",
              "Environmental measurements"
            ),

            textOutput("environment_count", inline = TRUE)
          )
        ),

        column(
          width = 3,

          div(
            class = "stat-card",

            div(
              class = "stat-label",
              "Microbial observations"
            ),

            textOutput("microbial_count", inline = TRUE)
          )
        )
      ),

      fluidRow(

        column(
          width = 12,

          div(
            class = "content-panel",

            h4(
              "Data overview",
              class = "section-title"
            ),

            tableOutput("table_summary")
          )
        )
      )
    ),

    # --------------------------------------------------------
    # Data import page
    # --------------------------------------------------------

    conditionalPanel(
      condition = "input.main_navigation == 'Data import'",

      fluidRow(

        column(
          width = 4,

          div(
            class = "sidebar-panel",

            h4(
              "Import One Health data",
              class = "section-title"
            ),

            fileInput(
              "human_file",
              "Human data",
              accept = c(".csv", ".xls", ".xlsx")
            ),

            fileInput(
              "animal_file",
              "Animal data",
              accept = c(".csv", ".xls", ".xlsx")
            ),

            fileInput(
              "environment_file",
              "Environmental data",
              accept = c(".csv", ".xls", ".xlsx")
            ),

            fileInput(
              "microbial_file",
              "Microbial observations",
              accept = c(".csv", ".xls", ".xlsx")
            ),

            actionButton(
              "import_data",
              "Import data",
              icon = icon("upload"),
              class = "btn-primary",
              width = "100%"
            ),

            hr(),

            h5("Import status"),

            verbatimTextOutput("import_status"),

            h5("Validation"),

            verbatimTextOutput("validation_status")
          )
        ),

        column(
          width = 8,

          div(
            class = "content-panel",

            h4(
              "Identifier overview",
              class = "section-title"
            ),

            tableOutput("identifier_summary")
          )
        )
      )
    ),

    # --------------------------------------------------------
    # Visualization page
    # --------------------------------------------------------

    conditionalPanel(
      condition = "input.main_navigation == 'Visualizations'",

      fluidRow(

        column(
          width = 12,

          div(
            class = "content-panel",

            h4(
              "Compartment distribution",
              class = "section-title"
            ),

            plotOutput(
              "compartment_plot",
              height = "380px"
            )
          )
        )
      ),

      fluidRow(

        column(
          width = 6,

          div(
            class = "content-panel",

            h4(
              "Microorganism observations",
              class = "section-title"
            ),

            plotOutput(
              "microbe_plot",
              height = "350px"
            )
          )
        ),

        column(
          width = 6,

          div(
            class = "content-panel",

            h4(
              "Microbiological status",
              class = "section-title"
            ),

            plotOutput(
              "microbial_status_plot",
              height = "350px"
            )
          )
        )
      ),

      fluidRow(

        column(
          width = 12,

          div(
            class = "content-panel",

            h4(
              "Environmental measurements",
              class = "section-title"
            ),

            selectInput(
              "environment_parameter",
              "Select environmental parameter",
              choices = character(0)
            ),

            plotOutput(
              "environment_plot",
              height = "350px"
            )
          )
        )
      )
    ),

    # --------------------------------------------------------
    # About page
    # --------------------------------------------------------

    conditionalPanel(
      condition = "input.main_navigation == 'About'",

      div(
        class = "content-panel",

        h3(
          "About OneHealthVizR",
          class = "section-title"
        ),

        p(
          "OneHealthVizR is an R package designed to support ",
          "the integration, validation, summarization and ",
          "visualization of human, animal and environmental ",
          "health data."
        ),

        h4("Scientific framework"),

        tags$ul(
          tags$li("Human health data"),
          tags$li("Animal health data"),
          tags$li("Environmental measurements"),
          tags$li("Microbiological observations")
        ),

        h4("Developer"),

        p(
          strong("Gbèna Ulrich Evrard LOKONON")
        ),

        p(
          "Molecular Biologist & Bioinformatician"
        ),

        p(
          "Creator and Developer of OneHealthVizR"
        )
      )
    ),

    # --------------------------------------------------------
    # Footer
    # --------------------------------------------------------

    div(
      class = "footer",

      p(
        strong("OneHealthVizR"),
        " | Integrated One Health Data Visualization"
      ),

      p(
        "Created and developed by ",
        strong("Gbèna Ulrich Evrard LOKONON")
      ),

      tags$small(
        "Molecular Biologist & Bioinformatician"
      )
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {

  study_data <- reactiveVal(NULL)

  import_message <- reactiveVal(
    "No data imported yet."
  )

  validation_message <- reactiveVal(
    "Validation has not been performed."
  )

  # ----------------------------------------------------------
  # Logo
  # ----------------------------------------------------------

  output$brand_logo <- renderUI({

    logo_path <- system.file(
      "shiny",
      "www",
      "logo.jpeg",
      package = "OneHealthVizR"
    )

    if (
      nzchar(logo_path) &&
      file.exists(logo_path)
    ) {

      tags$img(
        src = "logo.jpeg",
        class = "brand-logo"
      )

    } else {

      div(
        style = "
          background:white;
          color:#167D8D;
          padding:12px;
          border-radius:10px;
          text-align:center;
          font-weight:800;
          font-size:20px;
        ",
        "OHV"
      )
    }
  })

  # ----------------------------------------------------------
  # Import data
  # ----------------------------------------------------------

  observeEvent(input$import_data, {

    uploaded_files <- list(
      human = input$human_file,
      animal = input$animal_file,
      environment = input$environment_file,
      microbial = input$microbial_file
    )

    available_files <- !vapply(
      uploaded_files,
      is.null,
      logical(1)
    )

    if (!any(available_files)) {

      study_data(NULL)

      import_message(
        "Please upload at least one data file."
      )

      validation_message(
        "No data available for validation."
      )

      return()
    }

    imported_study <- tryCatch({

      OneHealthVizR::oh_import(

        human = if (!is.null(input$human_file)) {
          input$human_file$datapath
        } else {
          NULL
        },

        animal = if (!is.null(input$animal_file)) {
          input$animal_file$datapath
        } else {
          NULL
        },

        environment = if (!is.null(input$environment_file)) {
          input$environment_file$datapath
        } else {
          NULL
        },

        microbial = if (!is.null(input$microbial_file)) {
          input$microbial_file$datapath
        } else {
          NULL
        }
      )

    }, error = function(e) {

      import_message(
        paste("Import failed:", conditionMessage(e))
      )

      validation_message(
        "Validation could not be performed."
      )

      NULL
    })

    if (is.null(imported_study)) {
      study_data(NULL)
      return()
    }

    study_data(imported_study)

    import_message(
      "Data imported successfully."
    )

    validation_result <- tryCatch({

      OneHealthVizR::oh_validate(imported_study)

      "Validation successful."

    }, error = function(e) {

      paste(
        "Validation failed:",
        conditionMessage(e)
      )
    })

    validation_message(validation_result)

    environment_data <- imported_study$environment

    if (
      !is.null(environment_data) &&
      "parameter" %in% names(environment_data)
    ) {

      parameters <- unique(
        as.character(environment_data$parameter)
      )

      parameters <- parameters[
        !is.na(parameters) &
          nzchar(trimws(parameters))
      ]

      updateSelectInput(
        session,
        "environment_parameter",
        choices = parameters,
        selected = if (length(parameters) > 0) {
          parameters[[1]]
        } else {
          character(0)
        }
      )

    } else {

      updateSelectInput(
        session,
        "environment_parameter",
        choices = character(0)
      )
    }

  })

  # ----------------------------------------------------------
  # Status outputs
  # ----------------------------------------------------------

  output$import_status <- renderText({
    import_message()
  })

  output$validation_status <- renderText({
    validation_message()
  })

  # ----------------------------------------------------------
  # Overview
  # ----------------------------------------------------------

  overview_data <- reactive({

    req(study_data())

    OneHealthVizR::oh_overview(
      study_data()
    )
  })

  output$table_summary <- renderTable({

    overview_data()$table_summary

  }, rownames = FALSE)

  output$identifier_summary <- renderTable({

    overview_data()$identifier_summary

  }, rownames = FALSE)

  # ----------------------------------------------------------
  # Statistics
  # ----------------------------------------------------------

  output$human_count <- renderText({

    req(study_data())

    data <- study_data()$human

    if (is.null(data)) 0 else nrow(data)
  })

  output$animal_count <- renderText({

    req(study_data())

    data <- study_data()$animal

    if (is.null(data)) 0 else nrow(data)
  })

  output$environment_count <- renderText({

    req(study_data())

    data <- study_data()$environment

    if (is.null(data)) 0 else nrow(data)
  })

  output$microbial_count <- renderText({

    req(study_data())

    data <- study_data()$microbial

    if (is.null(data)) 0 else nrow(data)
  })

  # ----------------------------------------------------------
  # Compartment visualization
  # ----------------------------------------------------------

  output$compartment_plot <- renderPlot({

    req(study_data())

    OneHealthVizR::oh_plot_compartments(
      study_data()
    )
  })

  # ----------------------------------------------------------
  # Microorganisms
  # ----------------------------------------------------------

  output$microbe_plot <- renderPlot({

    req(study_data())

    microbial_data <- study_data()$microbial

    validate(
      need(
        !is.null(microbial_data) &&
          nrow(microbial_data) > 0,
        "No microbial observations available."
      )
    )

    OneHealthVizR::oh_plot_microbes(
      study_data()
    )
  })

  # ----------------------------------------------------------
  # Microbiological status
  # ----------------------------------------------------------

  output$microbial_status_plot <- renderPlot({

    req(study_data())

    microbial_data <- study_data()$microbial

    validate(
      need(
        !is.null(microbial_data) &&
          nrow(microbial_data) > 0,
        "No microbial observations available."
      )
    )

    OneHealthVizR::oh_plot_microbiological_status(
      study_data()
    )
  })

  # ----------------------------------------------------------
  # Environment
  # ----------------------------------------------------------

  output$environment_plot <- renderPlot({

    req(study_data())

    environment_data <- study_data()$environment

    validate(
      need(
        !is.null(environment_data) &&
          nrow(environment_data) > 0,
        "No environmental measurements available."
      )
    )

    validate(
      need(
        !is.null(input$environment_parameter) &&
          nzchar(input$environment_parameter),
        "Select an environmental parameter."
      )
    )

    OneHealthVizR::oh_plot_environment(
      study_data(),
      parameter = input$environment_parameter
    )
  })

}

# ------------------------------------------------------------
# Run application
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
