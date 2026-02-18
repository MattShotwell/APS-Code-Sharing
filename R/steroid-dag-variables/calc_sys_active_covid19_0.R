## -----------------------------------------------------------------------------
## Active SARS-CoV-2 Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Active SARS-CoV-2 Infection' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
calc_sys_active_covid19_0 <- function(has_pos_covid_0) {
  dplyr::case_when(
    has_pos_covid_0 == 1 ~ 1,
    is.na(has_pos_covid_0) ~ 0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_active_covid19_0 columns (one row per record_id)
wrapper_calc_sys_active_covid19_0 <- function(data, dictionary) {
  # Get enrollment times
  data_enrollment <- data |>
    filter(event_label == 'Day 0') |>
    select(record_id, enrollment_time)

  # Get all positive COVID-19 tests on or before enrollment
  data_positive_covid <- data |>
    filter(event_label == 'Hospital Discharge and Summary') |>
    left_join(
      get_code_label_map('cxpos', dictionary),
      by = 'cxpos'
    ) |>
    left_join(
      get_code_label_map('resp_pathogen', dictionary),
      by = 'resp_pathogen'
    ) |>
    filter(cxpos_code == 3 & resp_pathogen_code == 6) |>
    select(record_id, pathogen_date) |>
    left_join(data_enrollment, by = 'record_id') |>
    # Calculate if positive test was on or before enrollment
    mutate(positive_before_enrollment = pathogen_date <= enrollment_time) |>
    # Keep only tests on or before enrollment
    filter(positive_before_enrollment) |>
    # Group by patient and check if ANY test was positive before enrollment
    group_by(record_id) |>
    summarise(has_pos_covid_0 = 1, .groups = 'drop')

  # Create final classification for variable
  data_active_covid19 <- data_enrollment |>
    left_join(data_positive_covid, by = 'record_id') |>
    mutate(sys_active_covid19_0 = calc_sys_active_covid19_0(
      has_pos_covid_0 = has_pos_covid_0
    )) |>
    select(record_id, sys_active_covid19_0)

  data |>
    distinct(record_id) |>
    left_join(data_active_covid19, by = 'record_id')
}
