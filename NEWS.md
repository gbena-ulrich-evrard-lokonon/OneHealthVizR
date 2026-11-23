# OneHealthVizR 0.1.0

## Initial development release

### Core functionality

- Added `oh_import()` for importing human, animal, environmental, and microbial observation datasets.
- Added `oh_validate()` for checking dataset structure and identifier integrity.
- Added `oh_merge()` for integrating related One Health datasets.
- Added `oh_transform()` for preparing integrated data.
- Added `oh_overview()` for generating study-level summaries.
- Added visualization functions:
  - `oh_plot_compartments()`
  - `oh_plot_microbes()`
  - `oh_plot_microbiological_status()`
  - `oh_plot_environment()`

### Interactive application

- Added `run_onehealthvizr()` to launch the Shiny application.
- Added an interface for importing, validating, exploring, and visualizing One Health datasets.

### Data support

- Added support for CSV and Excel input files.
- Added synthetic example datasets covering human, animal, environmental, and microbial observations.
- Added a combined Excel demonstration workbook.

### Testing and documentation

- Added automated tests using `testthat`.
- Added package documentation using `roxygen2`.
- Added initial package usage examples and development documentation.
-
