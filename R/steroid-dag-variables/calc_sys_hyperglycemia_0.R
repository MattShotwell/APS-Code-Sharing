## -----------------------------------------------------------------------------
## Hyperglycemia (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Hyperglycemia: History' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_hyperglyc_hist_0 <- function(
  m_endocrine,
  m_endo_conditions___1
) {
  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___1) ~ 0,
    m_endocrine == "Yes" & is_checked(m_endo_conditions___1) ~ 1,
    is_unknown(m_endocrine) ~ 99
  )
}


# Calculate SYSTEMATIC DAG 'Hyperglycemia: Glucose Level' for Day 0
#
# Value: Glucose level in mg/dL (-1 = 'Not Collected')
calc_sys_hyperglyc_gluc_0 <- function(
  daily_gluc_8a_0,
  daily_gluc_nc_0
) {
  dplyr::case_when(
    !is.na(daily_gluc_8a_0) ~ daily_gluc_8a_0,
    daily_gluc_nc_0 == 'Not Collected' ~ -1
  )
}


# Convenience wrapper function
# Returns a data frame with record_id, sys_hyperglyc_hist_0, and sys_hyperglyc_gluc_0 columns (one row per record_id)
wrapper_calc_sys_hyperglycemia_0 <- function(data) {
  # Get history data from Day 0
  data_with_hist <- data |>
    filter(event_label == 'Day 0') |>
    mutate(sys_hyperglyc_hist_0 = calc_sys_hyperglyc_hist_0(
      m_endocrine = m_endocrine,
      m_endo_conditions___1 = m_endo_conditions___1
    )) |>
    select(record_id, sys_hyperglyc_hist_0)

  # Get glucose data from Daily In-Hospital Forms
  data_with_gluc <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    mutate(sys_hyperglyc_gluc_0 = calc_sys_hyperglyc_gluc_0(
      daily_gluc_8a_0 = daily_gluc_8a_0,
      daily_gluc_nc_0 = daily_gluc_nc_0
    )) |>
    select(record_id, sys_hyperglyc_gluc_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_hist, by = 'record_id') |>
    left_join(data_with_gluc, by = 'record_id')
}
