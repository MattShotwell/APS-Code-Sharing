## -----------------------------------------------------------------------------
## Steroid Responsive Comorbidity (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Steroid Responsive Comorbidity' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_steroid_comorb_0 <- function(
  mhrheumd,
  m_rheum_conditions___1,
  m_rheum_conditions___2,
  m_rheum_conditions___3,
  m_rheum_conditions___4,
  m_rheum_conditions___5,
  m_rheum_conditions___6,
  m_rheum_conditions___7,
  m_rheum_conditions___88,
  m_immunosuppression,
  m_immunosup_conditions___4,
  mhccster
) {
  dplyr::case_when(
    (mhrheumd == "No" &
      is_unchecked(m_rheum_conditions___1) &
      is_unchecked(m_rheum_conditions___2) &
      is_unchecked(m_rheum_conditions___3) &
      is_unchecked(m_rheum_conditions___4) &
      is_unchecked(m_rheum_conditions___5) &
      is_unchecked(m_rheum_conditions___6) &
      is_unchecked(m_rheum_conditions___7) &
      is_unchecked(m_rheum_conditions___88)) |
    (m_immunosuppression %in% c("Yes", "No") &
      is_unchecked(m_immunosup_conditions___4) &
      mhccster == "No") ~ 0,

    (mhrheumd == "Yes" & (
      is_checked(m_rheum_conditions___1) |
      is_checked(m_rheum_conditions___2) |
      is_checked(m_rheum_conditions___3) |
      is_checked(m_rheum_conditions___4) |
      is_checked(m_rheum_conditions___5) |
      is_checked(m_rheum_conditions___6) |
      is_checked(m_rheum_conditions___7) |
      is_checked(m_rheum_conditions___88))
      ) &
    ((m_immunosuppression == "Yes" &
      is_checked(m_immunosup_conditions___4)) |
      mhccster == "Yes") ~ 1,

    is_unknown(mhrheumd) | is_unknown(m_immunosuppression) | is_unknown(mhccster) ~ 99
  )
}

# Convenience wrapper function
# Returns a data frame with record_id and str_steroid_comorb_0 columns (one row per record_id)
wrapper_calc_str_steroid_comorb_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_steroid_comorb_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(str_steroid_comorb_0 = calc_str_steroid_comorb_0(
          mhrheumd = mhrheumd,
          m_rheum_conditions___1 = m_rheum_conditions___1,
          m_rheum_conditions___2 = m_rheum_conditions___2,
          m_rheum_conditions___3 = m_rheum_conditions___3,
          m_rheum_conditions___4 = m_rheum_conditions___4,
          m_rheum_conditions___5 = m_rheum_conditions___5,
          m_rheum_conditions___6 = m_rheum_conditions___6,
          m_rheum_conditions___7 = m_rheum_conditions___7,
          m_rheum_conditions___88 = m_rheum_conditions___88,
          m_immunosuppression = m_immunosuppression,
          m_immunosup_conditions___4 = m_immunosup_conditions___4,
          mhccster = mhccster
        )) |>
        select(record_id, str_steroid_comorb_0),
      by = 'record_id'
    )
}
