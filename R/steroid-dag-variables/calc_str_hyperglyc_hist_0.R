## -----------------------------------------------------------------------------
## Hyperglycemia (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Hyperglycemia' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
calc_str_hyperglyc_hist_0 <- function(
  daily_gluc_8a_0,
  daily_gluc_nc_0
) {
  dplyr::case_when(
    daily_gluc_8a_0 > 250 ~ 1,
    is.na(daily_gluc_8a_0) | (daily_gluc_8a_0 <= 250) ~ 0,
    daily_gluc_nc_0 == 'Not Collected' ~ 0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_hyperglyc_hist_0 columns (one row per record_id)
wrapper_calc_str_hyperglyc_hist_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_hyperglyc_hist_0 and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        mutate(str_hyperglyc_hist_0 = calc_str_hyperglyc_hist_0(
          daily_gluc_8a_0 = daily_gluc_8a_0,
          daily_gluc_nc_0 = daily_gluc_nc_0
        )) |>
        select(record_id, str_hyperglyc_hist_0),
      by = 'record_id'
    )
}
