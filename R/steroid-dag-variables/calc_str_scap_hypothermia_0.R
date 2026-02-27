## -----------------------------------------------------------------------------
## SCAP Criterion: Hypothermia (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: Hypothermia'
#
# Values:
# - 0 = No (Temperature >= 35.0°C)
# - 1 = Yes (Temperature < 35.0°C)
calc_str_scap_hypothermia_0 <- function(
  lowtemp_vsorres
  ) {

  ## Apply SCAP criterion
  dplyr::case_when(
    lowtemp_vsorres >= 35.0 ~ 0,
    lowtemp_vsorres < 35.0 ~ 1
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_scap_hypothermia_0 columns (one row per record_id)
wrapper_calc_str_scap_hypothermia_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_scap_hypothermia_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(str_scap_hypothermia_0 = calc_str_scap_hypothermia_0(
          lowtemp_vsorres = lowtemp_vsorres
        )) |>
        select(record_id, str_scap_hypothermia_0),
      by = 'record_id'
    )
}
