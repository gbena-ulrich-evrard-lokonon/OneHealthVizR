# ============================================================
# OneHealthVizR
# File: helper-fixtures.R
# Purpose: Shared test data and helper functions.
# ============================================================

create_valid_study <- function() {

  human_data <- data.frame(
    study_id = "STUDY001",
    host_id = "H001",
    site_id = "SITE001",
    event_id = "EVENT001",
    sample_id = "SAMPLE001",
    age = "25",
    sex = "Female",
    occupation = "Farmer",
    sample_type = "Blood",
    collection_date = "2026-05-12",
    infection_status = "Positive"
  )

  file_path <- tempfile(fileext = ".csv")

  utils::write.csv(
    human_data,
    file = file_path,
    row.names = FALSE
  )

  on.exit(unlink(file_path), add = TRUE)

  oh_import(human = file_path)
}

create_streptococcus_study <- function() {

  human <- data.frame(
    study_id = c("STREP001", "STREP001"),
    host_id = c("H001", "H002"),
    site_id = c("SITE001", "SITE001"),
    event_id = c("EVENT001", "EVENT001"),
    sample_id = c("H-S01", "H-S02"),
    age = c("34", "41"),
    sex = c("Male", "Female"),
    occupation = c("Farmer", "Butcher"),
    sample_type = c("Blood", "Blood"),
    collection_date = c("2026-03-10", "2026-03-11"),
    infection_status = c("Positive", "Negative")
  )

  animal <- data.frame(
    study_id = rep("STREP001", 3),
    host_id = c("P001", "P002", "P003"),
    site_id = rep("SITE001", 3),
    event_id = rep("EVENT001", 3),
    sample_id = c("A-S01", "A-S02", "A-S03"),
    species = rep("Sus scrofa domesticus", 3),
    age = c("8 months", "10 months", "7 months"),
    sex = c("Male", "Female", "Male"),
    production_type = rep("Farm", 3),
    sample_type = rep("Nasal swab", 3),
    collection_date = c(
      "2026-03-10",
      "2026-03-10",
      "2026-03-11"
    ),
    infection_status = c("Positive", "Positive", "Negative")
  )

  environment <- data.frame(
    measurement_id = c("M001", "M002", "M003"),
    study_id = rep("STREP001", 3),
    site_id = rep("SITE001", 3),
    event_id = rep("EVENT001", 3),
    sample_id = rep("E-S01", 3),
    parameter = c("Temperature", "Humidity", "pH"),
    value = c("28.5", "75", "6.8"),
    unit = c("Celsius", "%", "pH"),
    measurement_date = rep("2026-03-10", 3),
    method = c("Thermometer", "Hygrometer", "pH meter")
  )

  microbial <- data.frame(
    observation_id = c("OBS001", "OBS002", "OBS003", "OBS004", "OBS005"),
    study_id = rep("STREP001", 5),
    sample_id = c("H-S01", "H-S02", "A-S01", "A-S02", "E-S01"),
    microorganism_id = rep("STRSUIS001", 5),
    method = rep("PCR", 5),
    result_type = rep("Qualitative", 5),
    result_status = c(
      "Positive",
      "Negative",
      "Positive",
      "Positive",
      "Positive"
    ),
    result_value = c("Detected", "Not detected", "Detected", "Detected", "Detected"),
    result_unit = rep(NA_character_, 5)
  )

  temp_files <- lapply(
    c("human", "animal", "environment", "microbial"),
    function(name) tempfile(
      pattern = paste0(name, "_"),
      fileext = ".csv"
    )
  )

  names(temp_files) <- c(
    "human",
    "animal",
    "environment",
    "microbial"
  )

  utils::write.csv(human, temp_files$human, row.names = FALSE, na = "")
  utils::write.csv(animal, temp_files$animal, row.names = FALSE, na = "")
  utils::write.csv(environment, temp_files$environment, row.names = FALSE, na = "")
  utils::write.csv(microbial, temp_files$microbial, row.names = FALSE, na = "")

  on.exit(
    unlink(unlist(temp_files)),
    add = TRUE
  )

  oh_import(
    human = temp_files$human,
    animal = temp_files$animal,
    environment = temp_files$environment,
    microbial = temp_files$microbial
  )
}
