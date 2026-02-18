## -----------------------------------------------------------------------------
## Inflammatory Profile (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Inflammatory Profile'
#
# Values:
# - 0 = CRP not checked
# - 1 = CRP checked and < 15 mg/L
# - 2 = CRP checked and >= 15 mg/L
calc_str_inflamprofile_0 <- function(
  daily_crp_8a_0,
  daily_crp_nc_0,
  daily_crp_8a_m1,
  daily_crp_nc_m1,
  daily_crp_8a_m2,
  daily_crp_nc_m2
  ) {
  # Use the CRP component calculation from the SYSTEMATIC DAG
  calc_crp_0(
    daily_crp_8a_0,
    daily_crp_nc_0,
    daily_crp_8a_m1,
    daily_crp_nc_m1,
    daily_crp_8a_m2,
    daily_crp_nc_m2
  )
}

# Convenience wrapper function for STREAMLINED DAG
# Returns a data frame with record_id and str_inflamprofile_0 columns (one row per record_id)
wrapper_calc_str_inflamprofile_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_inflamprofile_0 and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        mutate(str_inflamprofile_0 = calc_str_inflamprofile_0(
          daily_crp_8a_0 = daily_crp_8a_0,
          daily_crp_nc_0 = daily_crp_nc_0,
          daily_crp_8a_m1 = daily_crp_8a_m1,
          daily_crp_nc_m1 = daily_crp_nc_m1,
          daily_crp_8a_m2 = daily_crp_8a_m2,
          daily_crp_nc_m2 = daily_crp_nc_m2
        )) |>
        select(record_id, str_inflamprofile_0),
      by = 'record_id'
    )
}
