# OneHealthVizR

[![R-CMD-check](https://github.com/gbena-ulrich-evrard-lokonon/OneHealthVizR/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/gbena-ulrich-evrard-lokonon/OneHealthVizR/actions/workflows/R-CMD-check.yml)

## Integrated One Health Data Visualization in R

OneHealthVizR is an R package designed to import, validate, integrate, summarize, and visualize data from human, animal, and environmental health compartments within an integrated One Health framework.

The package supports researchers, epidemiologists, microbiologists, and public health professionals in exploring interconnected health data through reproducible analytical workflows and accessible visualizations.

**Project status:** Under active development.

**Current development version:** 0.0.0.9000

OneHealthVizR is currently under active development. Its core functions are implemented and tested, but the API and features may evolve.

Key features

- Data import: Import human, animal, environmental, and microbiological observation datasets from CSV and Excel files.
- Data validation: Check data structure, required variables, identifiers, and relationships between datasets.
- Data integration: Bring related One Health datasets together while preserving their original structure.
- Data transformation: Prepare integrated data for downstream exploration.
- Study overview: Generate summaries of records, variables, missing values, and identifiers.
- Visualization: Explore One Health compartments, microorganisms, microbiological results, and environmental parameters.
- Interactive interface: Explore data through an integrated Shiny application.

One Health framework

OneHealthVizR organizes data around four complementary tables:

| Dataset | Description |
|---|---|
| Human | Human hosts, demographic information, samples, and infection status |
| Animal | Animal hosts, species, production information, samples, and infection status |
| Environment | Environmental measurements associated with sites, events, and samples |
| Microbial observations | Microorganisms detected or investigated in collected samples |

The package uses linked datasets rather than requiring all information to be stored in a single table.

This approach supports the exploration of relationships between human, animal, and environmental health while preserving the distinction between their respective data structures.

Installation

Install the development version from GitHub:

install.packages("devtools")

devtools::install_github(
  "gbena-ulrich-evrard-lokonon/OneHealthVizR"
)

Load the package:

library(OneHealthVizR)

For the interactive application, install Shiny if necessary:

install.packages("shiny")

Quick start

1. Import One Health datasets

OneHealthVizR accepts CSV and Excel files.

study <- oh_import(
  human = "human.csv",
  animal = "animal.csv",
  environment = "environment.csv",
  microbial = "microbial_observations.csv"
)

Each dataset is optional, depending on the available data and research objectives.

2. Validate the study

oh_validate(study)

This function checks the structure and integrity of the imported study.

3. Generate a study overview

overview <- oh_overview(study)

print(overview)

The overview summarizes the datasets, number of records, variables, missing values, and identifiers.

4. Integrate datasets

integrated_data <- oh_merge(study)

5. Transform data

transformed_data <- oh_transform(study)

6. Visualize One Health data

Explore the different dimensions of the study:

oh_plot_compartments(study)

oh_plot_microbes(study)

oh_plot_microbiological_status(study)

oh_plot_environment(
  study,
  parameter = "Temperature"
)

Available environmental parameters depend on the imported dataset.

Interactive Shiny application

OneHealthVizR includes an interactive application for importing, exploring, and visualizing One Health datasets.

Launch the application with:

run_onehealthvizr()

The interface provides:

- Dataset import;
- Study overview;
- Data validation feedback;
- Identifier exploration;
- Interactive visualization;
- Package information.

Example datasets

Synthetic datasets are provided in "inst/extdata/" to facilitate testing and exploration.

They include:

- "human.csv" and "human.xlsx"
- "animal.csv" and "animal.xlsx"
- "environment.csv" and "environment.xlsx"
- "microbial_observations.csv" and "microbial_observations.xlsx"
- "OneHealthVizR_demo.xlsx"

These datasets represent a fictional One Health study in Benin.

They are entirely synthetic and must not be interpreted as real epidemiological or microbiological observations.

To access the example files after installing the package:

human_file <- system.file(
  "extdata",
  "human.csv",
  package = "OneHealthVizR"
)

Data structure and principles

OneHealthVizR follows several principles:

1. Preserve the distinction between human, animal, and environmental data.
2. Maintain explicit identifiers and relationships between datasets.
3. Preserve missing values and distinguish them from true negative results and zero measurements.
4. Avoid silent data correction or automatic identifier harmonization.
5. Support reproducible and transparent data exploration.
6. Keep scientific processing independent of the Shiny interface.

The package does not automatically infer pathogen transmission or causal relationships from the detection of microorganisms across different compartments.

Testing

The package uses "testthat" for automated testing.

To run the test suite:

devtools::test()

To perform a package check:

devtools::check()

Dependencies

Imports

- "readr"
- "readxl"
- "ggplot2"

Suggests

- "shiny"
- "testthat"
- "roxygen2"

Author

Gbèna Ulrich Evrard LOKONON

Molecular Biologist & Bioinformatician

Email: ulrichlokn@gmail.com

GitHub: "gbena-ulrich-evrard-lokonon" (https://github.com/gbena-ulrich-evrard-lokonon)

Contributing

Contributions, suggestions, bug reports, and feature requests are welcome.

Please use the "GitHub Issues" (https://github.com/gbena-ulrich-evrard-lokonon/OneHealthVizR/issues) page to report problems or suggest improvements.

License

This project is licensed under the MIT License. See the "LICENSE" file for details.

Citation

If you use OneHealthVizR in your research, please cite the package using its bibliographic information in the "DESCRIPTION" file.

A formal software citation will be provided when the project reaches its first stable release.

---

OneHealthVizR — Bringing Human, Animal, and Environmental Health Data Together.
