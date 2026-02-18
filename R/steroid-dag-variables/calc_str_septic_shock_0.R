## -----------------------------------------------------------------------------
## Septic Shock (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Septic Shock'
#
# Values:
# - 0 = No
# - 1 = Yes
calc_str_septic_shock_0 <- function(
  sys_sepsis_0,
  daily_vasopressors_0___0
) {
  dplyr::case_when(
    sys_sepsis_0 == 1 & is_unchecked(daily_vasopressors_0___0) ~ 1,
    .default = 0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_septic_shock_0 columns (one row per record_id)
wrapper_calc_str_septic_shock_0 <- function(data, dictionary) {
  # Get sepsis status using the sepsis function
  data_with_sepsis <- wrapper_calc_sys_sepsis_0(data, dictionary)

  # Get vasopressor data from Daily In-Hospital Forms
  data_with_vasopressors <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    select(
      record_id,
      daily_vasopressors_0___0
    )

  data_with_str_septic_shock <- data_with_vasopressors |>
    left_join(data_with_sepsis, by = 'record_id') |>
    mutate(
      str_septic_shock_0 = calc_str_septic_shock_0(
        sys_sepsis_0 = sys_sepsis_0,
        daily_vasopressors_0___0 = daily_vasopressors_0___0
      )
    ) |>
    select(record_id, str_septic_shock_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_str_septic_shock, by = 'record_id')
}
