## -----------------------------------------------------------------------------
## Major Recent Surgery (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Major Recent Surgery'
#
# Values:
# - 0 = No
# - 1 = Yes
calc_str_major_surgery_0 <- function(
  daily_surgery_m2,
  daily_surgery_m1,
  daily_surgery_0
) {

  ## Check if surgery occurred on any of the three days
  had_surgery <- (
    daily_surgery_m2 == "Administered" |
    daily_surgery_m1 == "Administered" |
    daily_surgery_0 == "Administered"
  )

  ## Return binary indicator
  dplyr::case_when(
    had_surgery ~ 1,
    .default = 0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_major_surgery_0 columns (one row per record_id)
wrapper_calc_str_major_surgery_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_major_surgery_0 and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        mutate(str_major_surgery_0 = calc_str_major_surgery_0(
          daily_surgery_m2 = daily_surgery_m2,
          daily_surgery_m1 = daily_surgery_m1,
          daily_surgery_0 = daily_surgery_0
        )) |>
        select(record_id, str_major_surgery_0),
      by = 'record_id'
    )
}
