## -----------------------------------------------------------------------------
## Neuromuscular Blockade (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Neuromuscular Blockade'
#
# Values:
# - 0 = No
# - 1 = Yes
calc_sys_nmblockade_0 <- function(daily_paralysis_0, trx_0) {
  dplyr::case_when(
    daily_paralysis_0 == 'Administered' ~ 1,
    trx_0 == 'Available' & !is.na(daily_paralysis_0) ~ 0,
    trx_0 == 'Not Available' & is.na(daily_paralysis_0) ~ 0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_nmblockade_0 columns (one row per record_id)
wrapper_calc_sys_nmblockade_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate sys_nmblockade_0 and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        mutate(sys_nmblockade_0 = calc_sys_nmblockade_0(
          daily_paralysis_0 = daily_paralysis_0,
          trx_0 = trx_0
        )) |>
        select(record_id, sys_nmblockade_0),
      by = 'record_id'
    )
}
