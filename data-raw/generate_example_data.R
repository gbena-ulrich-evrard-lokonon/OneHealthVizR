# ============================================================
# OneHealthVizR
# File: data-raw/generate_example_data.R
# Purpose: Generate synthetic One Health datasets
# Author: Gbèna Ulrich Evrard LOKONON
# ============================================================

set.seed(2026)

# ------------------------------------------------------------
# 1. Dependencies
# ------------------------------------------------------------

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop(
    "Install writexl first: install.packages('writexl')"
  )
}

# ------------------------------------------------------------
# 2. Output directory
# ------------------------------------------------------------

output_dir <- file.path("inst", "extdata")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# 3. Study information
# ------------------------------------------------------------

study_id <- "OHV_BENIN_2026"

sites <- c(
  "COTONOU",
  "ABOMEY_CALAVI",
  "PORTO_NOVO",
  "ALLADA"
)

events <- paste0("EVENT", sprintf("%03d", 1:12))

# ------------------------------------------------------------
# 4. Human dataset
# ------------------------------------------------------------

n_human <- 30

human <- data.frame(
  study_id = study_id,
  host_id = paste0("HUM", sprintf("%03d", 1:n_human)),
  site_id = sample(sites, n_human, replace = TRUE),
  event_id = sample(events, n_human, replace = TRUE),
  sample_id = paste0("HS", sprintf("%03d", 1:n_human)),
  age = sample(18:75, n_human, replace = TRUE),
  sex = sample(c("Female", "Male"), n_human, replace = TRUE),
  occupation = sample(
    c("Farmer", "Trader", "Student", "Healthcare worker"),
    n_human,
    replace = TRUE
  ),
  sample_type = sample(
    c("Blood", "Stool", "Urine"),
    n_human,
    replace = TRUE
  ),
  collection_date = as.Date("2026-01-01") +
    sample(0:180, n_human, replace = TRUE),
  infection_status = sample(
    c("positive", "negative", "unknown"),
    n_human,
    replace = TRUE,
    prob = c(0.30, 0.60, 0.10)
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 5. Animal dataset
# ------------------------------------------------------------

n_animal <- 20

animal <- data.frame(
  study_id = study_id,
  host_id = paste0("ANI", sprintf("%03d", 1:n_animal)),
  site_id = sample(sites, n_animal, replace = TRUE),
  event_id = sample(events, n_animal, replace = TRUE),
  sample_id = paste0("AS", sprintf("%03d", 1:n_animal)),
  species = sample(
    c("Bos taurus", "Capra hircus", "Gallus gallus"),
    n_animal,
    replace = TRUE
  ),
  age = sample(1:12, n_animal, replace = TRUE),
  sex = sample(c("Female", "Male"), n_animal, replace = TRUE),
  production_type = sample(
    c("Livestock", "Poultry", "Small ruminant"),
    n_animal,
    replace = TRUE
  ),
  sample_type = sample(
    c("Blood", "Faeces", "Nasal swab"),
    n_animal,
    replace = TRUE
  ),
  collection_date = as.Date("2026-01-01") +
    sample(0:180, n_animal, replace = TRUE),
  infection_status = sample(
    c("positive", "negative", "unknown"),
    n_animal,
    replace = TRUE,
    prob = c(0.35, 0.55, 0.10)
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 6. Environmental dataset
# Long format: one row per measurement
# ------------------------------------------------------------

n_environment_samples <- 12

environment_sample_ids <- paste0(
  "ES",
  sprintf("%03d", 1:n_environment_samples)
)

environment_parameters <- c(
  "Temperature",
  "pH",
  "Dissolved_oxygen",
  "Turbidity"
)

environment <- expand.grid(
  sample_id = environment_sample_ids,
  parameter = environment_parameters,
  stringsAsFactors = FALSE
)

environment <- environment[
  sample(
    seq_len(nrow(environment)),
    nrow(environment)
  ),
]

environment$measurement_id <- paste0(
  "MEAS",
  sprintf("%03d", seq_len(nrow(environment)))
)

environment$study_id <- study_id

environment$site_id <- sample(
  sites,
  nrow(environment),
  replace = TRUE
)

environment$event_id <- sample(
  events,
  nrow(environment),
  replace = TRUE
)

environment$value <- vapply(
  environment$parameter,
  function(parameter) {
    switch(
      parameter,
      Temperature = round(runif(1, 24, 36), 2),
      pH = round(runif(1, 5.5, 8.5), 2),
      Dissolved_oxygen = round(runif(1, 2, 10), 2),
      Turbidity = round(runif(1, 1, 50), 2)
    )
  },
  numeric(1)
)

environment$unit <- ifelse(
  environment$parameter == "Temperature", "°C",
  ifelse(
    environment$parameter == "pH", "pH units",
    ifelse(
      environment$parameter == "Dissolved_oxygen",
      "mg/L",
      "NTU"
    )
  )
)

environment$measurement_date <- as.Date("2026-01-01") +
  sample(0:180, nrow(environment), replace = TRUE)

environment$method <- sample(
  c("Field sensor", "Laboratory analysis", "Portable meter"),
  nrow(environment),
  replace = TRUE
)

environment <- environment[, c(
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
)]

# ------------------------------------------------------------
# 7. Microbial observations
# One row = one microbial observation in one sample
# ------------------------------------------------------------

all_sample_ids <- c(
  human$sample_id,
  animal$sample_id,
  environment_sample_ids
)

microorganisms <- c(
  "ECOLI",
  "SALMONELLA",
  "K_PNEUMONIAE",
  "S_AUREUS",
  "ENTEROCOCCUS"
)

methods <- c(
  "PCR",
  "qPCR",
  "Culture",
  "Sequencing"
)

n_observations <- 120

microbial <- data.frame(
  observation_id = paste0(
    "OBS",
    sprintf("%03d", 1:n_observations)
  ),
  study_id = study_id,
  sample_id = sample(
    all_sample_ids,
    n_observations,
    replace = TRUE
  ),
  microorganism_id = sample(
    microorganisms,
    n_observations,
    replace = TRUE
  ),
  method = sample(
    methods,
    n_observations,
    replace = TRUE
  ),
  result_type = sample(
    c("qualitative", "quantitative"),
    n_observations,
    replace = TRUE
  ),
  result_status = sample(
    c("positive", "negative", "inconclusive", "not tested"),
    n_observations,
    replace = TRUE,
    prob = c(0.35, 0.45, 0.10, 0.10)
  ),
  stringsAsFactors = FALSE
)

microbial$result_value <- vapply(
  seq_len(n_observations),
  function(i) {

    status <- microbial$result_status[i]
    type <- microbial$result_type[i]

    if (status %in% c("inconclusive", "not tested")) {
      return(NA_character_)
    }

    if (type == "qualitative") {
      if (status == "positive") {
        return("Detected")
      } else {
        return("Not detected")
      }
    }

    if (status == "positive") {
      return(as.character(round(runif(1, 10, 10000), 2)))
    }

    "0"
  },
  character(1)
)

microbial$result_unit <- ifelse(
  microbial$result_type == "quantitative",
  "copies/mL",
  NA_character_
)

# ------------------------------------------------------------
# 8. Export CSV files
# ------------------------------------------------------------

write.csv(
  human,
  file.path(output_dir, "human.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  animal,
  file.path(output_dir, "animal.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  environment,
  file.path(output_dir, "environment.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  microbial,
  file.path(output_dir, "microbial_observations.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# 9. Export individual Excel files
# ------------------------------------------------------------

writexl::write_xlsx(
  human,
  file.path(output_dir, "human.xlsx")
)

writexl::write_xlsx(
  animal,
  file.path(output_dir, "animal.xlsx")
)

writexl::write_xlsx(
  environment,
  file.path(output_dir, "environment.xlsx")
)

writexl::write_xlsx(
  microbial,
  file.path(output_dir, "microbial_observations.xlsx")
)

# ------------------------------------------------------------
# 10. Export one combined Excel workbook
# ------------------------------------------------------------

writexl::write_xlsx(
  list(
    Human = human,
    Animal = animal,
    Environment = environment,
    Microbial = microbial
  ),
  file.path(output_dir, "OneHealthVizR_demo.xlsx")
)

# ------------------------------------------------------------
# 11. Summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat(" OneHealthVizR synthetic data generated\n")
cat("========================================\n\n")

cat("Human records:", nrow(human), "\n")
cat("Animal records:", nrow(animal), "\n")
cat("Environmental measurements:", nrow(environment), "\n")
cat("Microbial observations:", nrow(microbial), "\n\n")

cat("Files generated in:", output_dir, "\n")
