## -----------------------------------------------------------------------------
## Active Vasculitis (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Active Vasculitis'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_vasculitis_0 <- function(
  mhrheumd,
  m_rheum_conditions___8,
  m_immunosuppression,
  m_immunosup_conditions___4,
  mhccster
  ) {


  dplyr::case_when(
    ## Active vasculitis: has vasculitis AND (immunosuppression OR steroids)
    is_checked(m_rheum_conditions___8) &
      (is_checked(m_immunosup_conditions___4) | mhccster == "Yes") ~ 1,

    ## No active vasculitis: no vasculitis OR (no immunosuppression AND no steroids)
    (mhrheumd %in% c("No", "Yes") & is_unchecked(m_rheum_conditions___8)) |
      (
        m_immunosuppression %in% c("No", "Yes") &
        is_unchecked(m_immunosup_conditions___4) &
        mhccster == "No"
      ) ~ 0,

    ## Unknown status
    is_unknown(mhccster) | is_unknown(m_immunosuppression) | is_unknown(mhrheumd) ~ 99
  )
}

# Convenience wrapper function
# Returns a data frame with record_id and str_vasculitis_0 columns (one row per record_id)
wrapper_calc_str_vasculitis_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_vasculitis_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(str_vasculitis_0 = calc_str_vasculitis_0(
          mhrheumd = mhrheumd,
          m_rheum_conditions___8 = m_rheum_conditions___8,
          m_immunosuppression = m_immunosuppression,
          m_immunosup_conditions___4 = m_immunosup_conditions___4,
          mhccster = mhccster
        )) |>
        select(record_id, str_vasculitis_0),
      by = 'record_id'
    )
}
