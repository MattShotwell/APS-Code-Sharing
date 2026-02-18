## -----------------------------------------------------------------------------
## SCAP Criterion: Leukopenia (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: Leukopenia'
#
# Values:
# - 0 = No (WBC >= 4)
# - 1 = Yes (WBC < 4)
calc_str_scap_leukopenia_0 <- function(
  daily_wbc_8a_0,
  daily_wbc_8a_m1,
  daily_wbc_8a_m2
  ) {

  ## Get value with lookback (Day 0 -> Day -1 -> Day -2)
  wbc <- get_value_with_lookback(daily_wbc_8a_0, daily_wbc_8a_m1, daily_wbc_8a_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    wbc >= 4 ~ 0,
    wbc < 4 ~ 1
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_scap_leukopenia_0 columns (one row per record_id)
wrapper_calc_str_scap_leukopenia_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_scap_leukopenia_0 and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        mutate(str_scap_leukopenia_0 = calc_str_scap_leukopenia_0(
          daily_wbc_8a_0 = daily_wbc_8a_0,
          daily_wbc_8a_m1 = daily_wbc_8a_m1,
          daily_wbc_8a_m2 = daily_wbc_8a_m2
        )) |>
        select(record_id, str_scap_leukopenia_0),
      by = 'record_id'
    )
}
