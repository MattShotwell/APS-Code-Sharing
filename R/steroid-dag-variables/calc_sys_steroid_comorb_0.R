## -----------------------------------------------------------------------------
## Steroid Responsive Comorbidity (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Steroid Responsive Comorbidity' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_steroid_comorb_0 <- function(
  mhrheumd,
  m_rheum_conditions___1,
  m_rheum_conditions___2,
  m_rheum_conditions___3,
  m_rheum_conditions___4,
  m_rheum_conditions___5,
  m_rheum_conditions___6,
  m_rheum_conditions___7,
  m_rheum_conditions___8,
  m_rheum_conditions___88,
  m_pulmonary,
  m_pulm_conditions___6
  ) {
    dplyr::case_when(
      mhrheumd == "No" &
      is_unchecked(m_rheum_conditions___1) &
      is_unchecked(m_rheum_conditions___2) &
      is_unchecked(m_rheum_conditions___3) &
      is_unchecked(m_rheum_conditions___4) &
      is_unchecked(m_rheum_conditions___5) &
      is_unchecked(m_rheum_conditions___6) &
      is_unchecked(m_rheum_conditions___7) &
      is_unchecked(m_rheum_conditions___8) &
      is_unchecked(m_rheum_conditions___88) &
      m_pulmonary %in% c("Yes", "No") &
      is_unchecked(m_pulm_conditions___6) ~ 0,

      (mhrheumd == "Yes" & (
        is_checked(m_rheum_conditions___1) |
        is_checked(m_rheum_conditions___2) |
        is_checked(m_rheum_conditions___3) |
        is_checked(m_rheum_conditions___4) |
        is_checked(m_rheum_conditions___5) |
        is_checked(m_rheum_conditions___6) |
        is_checked(m_rheum_conditions___7) |
        is_checked(m_rheum_conditions___8) |
        is_checked(m_rheum_conditions___88)
        )
      ) |
      (m_pulmonary == "Yes" &
      is_checked(m_pulm_conditions___6)) ~ 1,

      is_unknown(mhrheumd) | is_unknown(m_pulmonary) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_steroid_comorb_0 columns (one row per record_id)
wrapper_calc_sys_steroid_comorb_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate sys_steroid_comorb_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(sys_steroid_comorb_0 = calc_sys_steroid_comorb_0(
          mhrheumd = mhrheumd,
          m_rheum_conditions___1 = m_rheum_conditions___1,
          m_rheum_conditions___2 = m_rheum_conditions___2,
          m_rheum_conditions___3 = m_rheum_conditions___3,
          m_rheum_conditions___4 = m_rheum_conditions___4,
          m_rheum_conditions___5 = m_rheum_conditions___5,
          m_rheum_conditions___6 = m_rheum_conditions___6,
          m_rheum_conditions___7 = m_rheum_conditions___7,
          m_rheum_conditions___8 = m_rheum_conditions___8,
          m_rheum_conditions___88 = m_rheum_conditions___88,
          m_pulmonary = m_pulmonary,
          m_pulm_conditions___6 = m_pulm_conditions___6
        )) |>
        select(record_id, sys_steroid_comorb_0),
      by = 'record_id'
    )
}
