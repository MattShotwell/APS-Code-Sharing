## -----------------------------------------------------------------------------
## SCAP Criterion: Thrombocytopenia (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: Thrombocytopenia'
#
# Values:
# - 0 = No (Platelets >= 100)
# - 1 = Yes (Platelets < 100)
calc_str_scap_thrombocytopenia_0 <- function(
  daily_platelet_8a_0,
  daily_platelet_8a_m1,
  daily_platelet_8a_m2
  ) {

  ## Get value with lookback (Day 0 -> Day -1 -> Day -2)
  platelets <- get_value_with_lookback(daily_platelet_8a_0, daily_platelet_8a_m1, daily_platelet_8a_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    platelets >= 100 ~ 0,
    platelets < 100 ~ 1
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_scap_thrombocytopenia_0 columns (one row per record_id)
wrapper_calc_str_scap_thrombocytopenia_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_scap_thrombocytopenia_0 and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        mutate(str_scap_thrombocytopenia_0 = calc_str_scap_thrombocytopenia_0(
          daily_platelet_8a_0 = daily_platelet_8a_0,
          daily_platelet_8a_m1 = daily_platelet_8a_m1,
          daily_platelet_8a_m2 = daily_platelet_8a_m2
        )) |>
        select(record_id, str_scap_thrombocytopenia_0),
      by = 'record_id'
    )
}
